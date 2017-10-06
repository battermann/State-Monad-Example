package impure

import org.scalatest.FunSuite
import play.api.libs.json.Json

class SchemaExtractorTests extends FunSuite {
  test("test impure schema extractor") {
    val json =
      """{
        |  "value1": 42,
        |  "value2": { "value2.1": "", "value2.2": 5, "value2.3": { "value2.3.1": 1.0, "value2.3.2": 1.0, "value2.3.3": 1.0 } },
        |  "value3": { "value3.1": true, "value3.2": 2 },
        |  "value4": [
        |     [ { "value4.1": "", "value4.2": 123 }, { "value4.1": "", "value4.2": 345 } ],
        |     [ { "value4.1": "", "value4.2": 678 }, { "value4.1": "", "value4.2": 312 } ]
        |  ]
        |}
      """.stripMargin

    val schema = SchemaExtractor.fromJsRoot(Json.parse(json))

    schema match {
      case SchemaObject(_, name, fields) =>
        assert(name == "root")
        assert(fields.map(_._1) == List("value1", "value2", "value3", "value4"))
      case _ => fail()
    }
  }
}
