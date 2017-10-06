package impure

import java.util.UUID

import impure.Schema.ClassName
import play.api.libs.json._

object SchemaExtractor {

  def fromJsRoot(value: JsValue): Schema = {
    value match {
      case JsObject(fields) =>
        fromJsObject("root", fields.toList)
      case _ =>
        ???
    }
  }

  private def fromJsObject(name: ClassName, fields: List[(String, JsValue)]): Schema = {
    val schemaFields =
      fields.map { case (fieldName, value) =>
        fromJsValue(fieldName, value)
      }

    SchemaObject(
      UUID.randomUUID,
      name,
      schemaFields)
  }

  private def fromJsValue(name: String, value: JsValue): (String, Schema) = {
    val schema = value match {
      case JsNull =>
        ???
      case JsString(_) =>
        SchemaString
      case JsNumber(_) =>
        SchemaDouble
      case JsBoolean(_) =>
        SchemaBoolean
      case JsObject(fields) =>
        fromJsObject(name, fields.toList)
      case JsArray(values) =>
        val schemas =
          values.map(v => fromJsValue(name, v)._2)
        val first = schemas.head
        if (schemas forall Schema.haveSameStructure(first)) {
          SchemaArray(first)
        } else {
          ???
        }
    }

    (name, schema)
  }
}
