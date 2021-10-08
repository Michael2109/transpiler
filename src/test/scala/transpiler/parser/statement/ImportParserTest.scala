package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast._

import scala.collection.mutable.ArrayBuffer

class ImportParserTest extends AnyFunSpec with Matchers {
  describe("Import parser") {
    it("Should parse import x") {
      val code = "import x"
      val Parsed.Success(value, _) = parse(code, StatementParser.importParser(_))
      value shouldBe Import(ArrayBuffer(Name("x")))
    }

    it("Should parse import x.y") {
      val code = "import x.y"
      val Parsed.Success(value, _) = parse(code, StatementParser.importParser(_))
      value shouldBe Import(ArrayBuffer(Name("x"), Name("y")))
    }

    it("Should parse import x.y.z.a.b.c") {
      val code = "import x.y.z.a.b.c"
      val Parsed.Success(value, _) = parse(code, StatementParser.importParser(_))
      value shouldBe Import(ArrayBuffer(Name("x"), Name("y"), Name("z"), Name("a"), Name("b"), Name("c")))
    }

    it("Should parse import abc.xyz.Name") {
      val code = "import abc.xyz.Name"
      val Parsed.Success(value, _) = parse(code, StatementParser.importParser(_))
      value shouldBe Import(ArrayBuffer(Name("abc"), Name("xyz"), Name("Name")))
    }

    it("Should parse import _abc.xy_z.Name_") {
      val code = "import _abc.xy_z.Name_"
      val Parsed.Success(value, _) = parse(code, StatementParser.importParser(_))
      value shouldBe Import(ArrayBuffer(Name("_abc"), Name("xy_z"), Name("Name_")))
    }

    it("Should parse import a1b2c3.x1y2z3") {
      val code = "import a1b2c3.x1y2z3"
      val Parsed.Success(value, _) = parse(code, StatementParser.importParser(_))
      value shouldBe Import(ArrayBuffer(Name("a1b2c3"), Name("x1y2z3")))
    }
  }
}
