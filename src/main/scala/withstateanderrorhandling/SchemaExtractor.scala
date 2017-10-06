package withstateanderrorhandling

import cats.data.StateT._
import cats.data._
import cats.implicits._
import play.api.libs.json._
import withstateanderrorhandling.Schema.{ClassName, SchemaId}

object SchemaExtractor {

  type Error = String
  type ErrorOr[A] = Either[Error, A]
  type EitherState[A] = StateT[ErrorOr, SchemaId, A]

  def fromJsRoot(value: JsValue): EitherState[Schema] = {
    value match {
      case JsObject(fields) =>
        fromJsField("root", fields.toList)
      case _ =>
        lift[ErrorOr, SchemaId, Schema](Left("JSON root value must be an object"))
    }
  }

  private def fromJsField(name: ClassName, fields: List[(String, JsValue)]): EitherState[Schema] = {
    val schemaFieldsState =
      fields.map { case (fieldName, value) =>
        fromJsValue(fieldName, value)
      }

    for {
      schemaFields <- schemaFieldsState.sequence
      state <- get[ErrorOr, SchemaId]
      _ <- modify[ErrorOr, SchemaId](Schema.nextId)
    } yield
      SchemaObject(
        state,
        name,
        schemaFields)
  }

  private def fromJsValue(name: String, value: JsValue): EitherState[(String, Schema)] = {
    val schema = value match {
      case JsNull =>
        lift[ErrorOr, SchemaId, Schema](Left("cannot analyze type of a JSON null value"))
      case JsString(_) =>
        pure[ErrorOr, SchemaId, Schema](SchemaString)
      case JsNumber(_) =>
        pure[ErrorOr, SchemaId, Schema](SchemaDouble)
      case JsBoolean(_) =>
        pure[ErrorOr, SchemaId, Schema](SchemaBoolean)
      case JsObject(fields) =>
        fromJsField(name, fields.toList)
      case JsArray(values) =>
        fromJsArray(name, values.toList)
    }

    schema.map((name, _))
  }

  private def fromJsArray(name: String, values: List[JsValue]): EitherState[Schema] = {
    for {
      schemas <- values.map(v => fromJsValue(name, v).map(_._2)).sequence
      schema <- schemas.headOption match {
        case Some(first) if schemas forall Schema.haveSameStructure(first) =>
          pure[ErrorOr, SchemaId, Schema](SchemaArray(first))
        case None =>
          lift[ErrorOr, SchemaId, Schema](Left("cannot analyze empty JSON array"))
        case _ =>
          lift[ErrorOr, SchemaId, Schema](Left("array type is not consistent"))
      }
    } yield schema
  }
}
