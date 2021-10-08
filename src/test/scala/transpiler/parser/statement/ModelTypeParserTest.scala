package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST.{ClassModelType, ObjectModelType, TraitModelType}

class ModelTypeParserTest extends AnyFunSpec with Matchers {
  describe("Model Type Parser") {
    it("Should parse class model type") {
      val Parsed.Success(value, _) = parse("class", StatementParser.modelTypeParser(_))
      value shouldBe ClassModelType
    }

    it("Should parse trait model type") {
      val Parsed.Success(value, _) = parse("trait", StatementParser.modelTypeParser(_))
      value shouldBe TraitModelType
    }

    it("Should parse object model type") {
      val Parsed.Success(value, _) = parse("object", StatementParser.modelTypeParser(_))
      value shouldBe ObjectModelType
    }
  }
}
