package impure

import java.util.UUID

import impure.Schema._

sealed trait Schema

case class SchemaOption(schema: Schema) extends Schema
case object SchemaString extends Schema
case object SchemaBoolean extends Schema
case object SchemaDouble extends Schema
case class SchemaArray(schema: Schema) extends Schema
case class SchemaObject(id: SchemaId, name: ClassName, fields: Seq[Field]) extends Schema

object Schema {
  type ClassName = String
  type FieldName = String
  type Field = (FieldName, Schema)
  type SchemaId = UUID

  def haveSameStructure(a: Schema)(b: Schema): Boolean = {
    (a, b) match {
      case (SchemaObject(_, _, fields1), SchemaObject(_, _, fields2)) =>
        fields1.size == fields2.size && fields1.forall{ case (n1, e1) => fields2.exists { case (n2, e2) => n1 == n2 && haveSameStructure(e1)(e2) }}
      case (SchemaArray(schema1), SchemaArray(schema2)) =>
        haveSameStructure(schema1)(schema2)
      case (SchemaOption(schema1), SchemaOption(schema2)) =>
        haveSameStructure(schema1)(schema2)
      case _ => a == b
    }
  }
}