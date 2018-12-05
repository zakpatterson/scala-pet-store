package io.github.pauljamescleary.petstore

import config.{DatabaseConfig, PetStoreConfig}
import domain.users._
import domain.orders._
import domain.pets._
import infrastructure.endpoint.{OrderEndpoints, PetEndpoints, UserEndpoints}
import infrastructure.repository.doobie.{DoobieOrderRepositoryInterpreter, DoobiePetRepositoryInterpreter, DoobieUserRepositoryInterpreter}
import cats.effect._
import cats.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.jca.BCrypt
import scala.concurrent.ExecutionContext.Implicits.global

object Server extends IOApp {
  private val keyGen = HMACSHA256

  def createServer[F[_] : ContextShift : ConcurrentEffect : Timer]: Resource[F, ExitCode] =
    for {
      conf           <- Resource.liftF(PetStoreConfig.load[F])
      signingKey     <- Resource.liftF(keyGen.generateKey[F])
      xa             <- DatabaseConfig.dbTransactor(conf.db, global, global)
      petRepo        =  DoobiePetRepositoryInterpreter[F](xa)
      orderRepo      =  DoobieOrderRepositoryInterpreter[F](xa)
      userRepo       =  DoobieUserRepositoryInterpreter[F](xa)
      petValidation  =  PetValidationInterpreter[F](petRepo)
      userValidation =  UserValidationInterpreter[F](userRepo)
      services       =  {
        implicit val petService = PetService[F](petRepo, petValidation)
        implicit val orderService = OrderService[F](orderRepo)
        implicit val userService = UserService[F](userRepo, userValidation)
        implicit val pwHasher = BCrypt.syncPasswordHasher[F]

        PetEndpoints.endpoints[F] <+>
          OrderEndpoints.endpoints[F] <+>
          UserEndpoints.endpoints[F, BCrypt]
      }
      httpApp = Router("/" -> services).orNotFound
      _ <- Resource.liftF(DatabaseConfig.initializeDb(conf.db))
      exitCode <- Resource.liftF(
        BlazeServerBuilder[F]
        .bindHttp(8080, "localhost")
        .withHttpApp(httpApp)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
      )
    } yield exitCode

  def run(args : List[String]) : IO[ExitCode] = createServer.use(IO.pure)
}
