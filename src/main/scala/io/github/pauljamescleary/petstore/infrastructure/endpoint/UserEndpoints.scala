package io.github.pauljamescleary.petstore
package infrastructure.endpoint

import cats.data.EitherT
import cats.effect.Effect
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, HttpRoutes}

import scala.language.higherKinds
import domain._
import domain.users._
import domain.authentication._
import tsec.common.Verified
import tsec.passwordhashers.{PasswordHash, PasswordHasher}

class UserEndpoints[F[_]: Effect : UserService, A](implicit cryptService : PasswordHasher[F, A]) extends Http4sDsl[F] {
  val S : UserService[F] = implicitly
  import Pagination._
  /* Jsonization of our User type */

  implicit val userDecoder: EntityDecoder[F, User] = jsonOf
  implicit val loginReqDecoder: EntityDecoder[F, LoginRequest] = jsonOf

  implicit val signupReqDecoder: EntityDecoder[F, SignupRequest] = jsonOf

  private def loginEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "login" =>
        val action: EitherT[F, UserAuthenticationFailedError, User] = for {
          login <- EitherT.liftF(req.as[LoginRequest])
          name = login.userName
          user <- S.getUserByName(name).leftMap(_ => UserAuthenticationFailedError(name))
          checkResult <- EitherT.liftF(cryptService.checkpw(login.password, PasswordHash[A](user.hash)))
          resp <-
            if(checkResult == Verified) EitherT.rightT[F, UserAuthenticationFailedError](user)
            else EitherT.leftT[F, User](UserAuthenticationFailedError(name))
        } yield resp

        action.value.flatMap {
          case Right(user) => Ok(user.asJson)
          case Left(UserAuthenticationFailedError(name)) => BadRequest(s"Authentication failed for user $name")
        }
    }

  private def signupEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "users" =>
        val action = for {
          signup <- req.as[SignupRequest]
          hash <- cryptService.hashpw(signup.password)
          user <- signup.asUser(hash).pure[F]
          result <- S.createUser(user).value
        } yield result

        action.flatMap {
          case Right(saved) => Ok(saved.asJson)
          case Left(UserAlreadyExistsError(existing)) =>
            Conflict(s"The user with user name ${existing.userName} already exists")
        }
    }

  private def updateEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ PUT -> Root / "users" / name =>
        val action = for {
          user <- req.as[User]
          updated = user.copy(userName = name)
          result <- S.update(updated).value
        } yield result

        action.flatMap {
          case Right(saved) => Ok(saved.asJson)
          case Left(UserNotFoundError) => NotFound("User not found")
        }
    }

  private def listEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "users" :? PageSizeMatcher(pageSize) :? OffsetMatcher(offset) =>
        for {
          retrived <- S.list(pageSize, offset)
          resp <- Ok(retrived.asJson)
        } yield resp
    }

  private def searchByNameEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "users" / userName =>
        S.getUserByName(userName).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case Left(UserNotFoundError) => NotFound("The user was not found")
        }
    }

  private def deleteUserEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case DELETE -> Root / "users" / userName =>
        for {
          _ <- S.deleteByUserName(userName)
          resp <- Ok()
        } yield resp
    }

  def endpoints: HttpRoutes[F] =
    loginEndpoint <+>
    signupEndpoint <+>
    updateEndpoint <+>
    listEndpoint   <+>
    searchByNameEndpoint   <+>
    deleteUserEndpoint
}

object UserEndpoints {
  def endpoints[F[_]: Effect : UserService, A](implicit cryptService: PasswordHasher[F, A]): HttpRoutes[F] =
    new UserEndpoints[F, A].endpoints
}
