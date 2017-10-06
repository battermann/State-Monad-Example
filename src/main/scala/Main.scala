import cats.implicits._
import play.api.libs.json.Json

object Main extends App {

  val json =
    """{
      |  "value1": 42,
      |  "value2": { "value2.1": "", "value2.2": 5, "value2.3": { "value2.3.1": 1.0, "value2.3.2": 1.0, "value2.3.3": 1.0 } },
      |  "value3": { "value3.1": 1, "value3.2": 2 },
      |  "value4": [
      |     [ { "value4.1": "", "value4.2": 123 }, { "value4.1": "", "value4.2": 345 } ],
      |     [ { "value4.1": "", "value4.2": 678 }, { "value4.1": "", "value4.2": 312 } ]
      |  ]
      |}
    """.stripMargin

  val schema1 = impure.SchemaExtractor.fromJsRoot(Json.parse(json))

  pprint.pprintln(schema1)

  val schema2 = withstate.SchemaExtractor.fromJsRoot(Json.parse(json))
  //pprint.pprintln(schema2.runA(UUID.fromString("f5c6869b-09cc-4727-8cc7-7cad75b9b587")).value)
  pprint.pprintln(schema2.runA(0).value)

  val schema3 = withstateanderrorhandling.SchemaExtractor.fromJsRoot(Json.parse(json))
  pprint.pprintln(schema3.runA(0))
}
