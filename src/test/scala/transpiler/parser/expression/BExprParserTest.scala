package transpiler.parser.expression

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.StatementParser.expressionParser
import transpiler.parser.ast._


class BExprParserTest extends AnyFunSpec with Matchers {
  describe("Boolean statementParser parsers") {
    it("Should parse boolean constant `true`") {
      val Parsed.Success(value, _) = parse("true", expressionParser(_))
      value shouldBe BoolConst(true)

    }
    it("Should parse boolean constant `false`") {
      val Parsed.Success(value, _) = parse("false", expressionParser(_))
      value shouldBe BoolConst(false)
    }
  }

  describe("Relational statementParser parsers") {
    it("Should parse `greater than`") {
      val Parsed.Success(value, _) = parse("x > 10", expressionParser(_))
      value shouldBe RBinary(Greater, Identifier(Name("x")), IntConst(10))
    }
    it("Should parse `less than`") {
      val Parsed.Success(value, _) = parse("x <10", expressionParser(_))
      value shouldBe RBinary(Less, Identifier(Name("x")), IntConst(10))
    }
    it("Should parse `less than equal`") {
      val Parsed.Success(value, _) = parse("x>= 10", expressionParser(_))
      value shouldBe RBinary(GreaterEqual, Identifier(Name("x")), IntConst(10))
    }
    it("Should parse `greater than equal`") {
      val Parsed.Success(value, _) = parse("x <=10", expressionParser(_))
      value shouldBe RBinary(LessEqual, Identifier(Name("x")), IntConst(10))
    }
  }
}
