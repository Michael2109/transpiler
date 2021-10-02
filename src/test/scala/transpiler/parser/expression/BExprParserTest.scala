package transpiler.parser.expression

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.ast.AST._
import transpiler.utils.TestUtil

class BExprParserTest extends AnyFunSpec with Matchers {
  describe("Boolean statementParser parsers") {
    it("Should parse boolean constant `true`") {
      TestUtil.parse("true", ExpressionParser.expressionParser) shouldBe Identifier(Name("true"))
    }
    it("Should parse boolean constant `false`") {
      TestUtil.parse("false", ExpressionParser.expressionParser) shouldBe Identifier(Name("false"))
    }
  }

  describe("Relational statementParser parsers") {
    it("Should parse `less than`") {
      TestUtil.parse("x > 10", ExpressionParser.expressionParser) shouldBe RBinary(Greater, Identifier(Name("x")), IntConst(10))
    }
    it("Should parse `greater than`") {
      TestUtil.parse("x < 10", ExpressionParser.expressionParser) shouldBe RBinary(Less, Identifier(Name("x")), IntConst(10))
    }
    it("Should parse `less than equal`") {
      TestUtil.parse("x >= 10", ExpressionParser.expressionParser) shouldBe RBinary(GreaterEqual, Identifier(Name("x")), IntConst(10))
    }
    it("Should parse `greater than equal`") {
      TestUtil.parse("x <= 10", ExpressionParser.expressionParser) shouldBe RBinary(LessEqual, Identifier(Name("x")), IntConst(10))
    }
  }
}
