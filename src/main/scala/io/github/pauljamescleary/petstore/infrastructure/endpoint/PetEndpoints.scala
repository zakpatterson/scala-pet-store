package io.github.pauljamescleary.petstore.infrastructure.endpoint

import cats.data.Validated.Valid
import cats.data._
import cats.effect.Effect
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, HttpRoutes, QueryParamDecoder}
import scala.language.higherKinds

import io.github.pauljamescleary.petstore.domain.{PetAlreadyExistsError, PetNotFoundError}
import io.github.pauljamescleary.petstore.domain.pets.{Pet, PetService, PetStatus}

class PetEndpoints[F[_]: Effect : PetService] extends Http4sDsl[F] {
  import Pagination._
  val S : PetService[F] = implicitly

  /* Parses out status query param which could be multi param */
  implicit val statusQueryParamDecoder: QueryParamDecoder[PetStatus] =
    QueryParamDecoder[String].map(PetStatus.withName)

  /* Relies on the statusQueryParamDecoder implicit, will parse out a possible multi-value query parameter */
  object StatusMatcher extends OptionalMultiQueryParamDecoderMatcher[PetStatus]("status")

  /* Parses out tag query param, which could be multi-value */
  object TagMatcher extends OptionalMultiQueryParamDecoderMatcher[String]("tags")

  implicit val petDecoder: EntityDecoder[F, Pet] = jsonOf[F, Pet]

  private def createPetEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "pets" =>
        val action = for {
          pet <- req.as[Pet]
          result <- S.create(pet).value
        } yield result

        action.flatMap {
          case Right(saved) =>
            Ok(saved.asJson)
          case Left(PetAlreadyExistsError(existing)) =>
            Conflict(s"The pet ${existing.name} of category ${existing.category} already exists")
        }
    }

  private def updatePetEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ PUT -> Root / "pets" / LongVar(petId) =>
        val action = for {
          pet <- req.as[Pet]
          updated = pet.copy(id = Some(petId))
          result <- S.update(pet).value
        } yield result

        action.flatMap {
          case Right(saved) => Ok(saved.asJson)
          case Left(PetNotFoundError) => NotFound("The pet was not found")
        }
    }

  private def getPetEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "pets" / LongVar(id) =>
        S.get(id).value.flatMap {
          case Right(found) => Ok(found.asJson)
          case Left(PetNotFoundError) => NotFound("The pet was not found")
        }
    }

  private def deletePetEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case DELETE -> Root / "pets" / LongVar(id) =>
        for {
          _ <- S.delete(id)
          resp <- Ok()
        } yield resp
    }

  private def listPetsEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "pets" :? PageSizeMatcher(pageSize) :? OffsetMatcher(offset) =>
        for {
          retrieved <- S.list(pageSize, offset)
          resp <- Ok(retrieved.asJson)
        } yield resp
    }

  private def findPetsByStatusEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "pets" / "findByStatus" :? StatusMatcher(Valid(Nil)) =>
        // User did not specify any statuses
        BadRequest("status parameter not specified")

      case GET -> Root / "pets" / "findByStatus" :? StatusMatcher(Valid(statuses)) =>
        // We have a list of valid statuses, find them and return
        for {
          retrieved <- S.findByStatus(NonEmptyList.fromListUnsafe(statuses))
          resp <- Ok(retrieved.asJson)
        } yield resp
    }

  private def findPetsByTagEndpoint: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / "pets" / "findByTags" :? TagMatcher(Valid(Nil)) =>
        BadRequest("tag parameter not specified")

      case GET -> Root / "pets" / "findByTags" :? TagMatcher(Valid(tags)) =>
        for {
          retrieved <- S.findByTag(NonEmptyList.fromListUnsafe(tags))
          resp <- Ok(retrieved.asJson)
        } yield resp

    }

  def endpoints: HttpRoutes[F] =
    createPetEndpoint <+>
      getPetEndpoint <+>
      deletePetEndpoint <+>
      listPetsEndpoint <+>
      findPetsByStatusEndpoint <+>
      updatePetEndpoint <+>
      findPetsByTagEndpoint
}

object PetEndpoints {
  def endpoints[F[_]: Effect : PetService]: HttpRoutes[F] = new PetEndpoints[F].endpoints
}
