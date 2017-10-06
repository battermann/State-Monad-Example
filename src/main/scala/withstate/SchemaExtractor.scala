package withstate

import cats.data.State._
import cats.data._
import cats.implicits._
import play.api.libs.json._
import withstate.Schema.{ClassName, SchemaId}

object SchemaExtractor {

  def fromJsRoot(value: JsValue): State[SchemaId, Schema] = {
    value match {
      case JsObject(fields) =>
        fromJsField("root", fields.toList)
      case _ =>
        ???
    }
  }

  private def fromJsField(name: ClassName, fields: List[(String, JsValue)]): State[SchemaId, Schema] = {
    val schemaFieldsState =
      fields.map { case (fieldName, value) =>
        fromJsValue(fieldName, value)
      }

    for {
      schemaFields <- schemaFieldsState.sequence
      state <- get[SchemaId]
      _ <- modify(Schema.nextId)
    } yield
      SchemaObject(
        state,
        name,
        schemaFields)
  }

  private def fromJsValue(name: String, value: JsValue): State[SchemaId, (String, Schema)] = {
    val schema = value match {
      case JsNull =>
        ???
      case JsString(_) =>
        pure[SchemaId, Schema](SchemaString)
      case JsNumber(_) =>
        pure[SchemaId, Schema](SchemaDouble)
      case JsBoolean(_) =>
        pure[SchemaId, Schema](SchemaBoolean)
      case JsObject(fields) =>
        fromJsField(name, fields.toList)
      case JsArray(values) =>
        fromJsArray(name, values.toList)
    }

    schema.map((name, _))
  }

  private def fromJsArray(name: String, values: List[JsValue]): State[SchemaId, Schema] = {
    for {
      schemas <- values.map(v => fromJsValue(name, v).map(_._2)).sequence
      first = schemas.head
    } yield
      if (schemas forall Schema.haveSameStructure(first)) {
        SchemaArray(first)
      } else {
        ???
      }
  }

}
