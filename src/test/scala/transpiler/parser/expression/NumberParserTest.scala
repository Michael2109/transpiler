package transpiler.parser.expression

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.ast.AST.{DoubleConst, FloatConst, IntConst, LongConst}
import transpiler.utils.TestUtil

class NumberParserTest extends AnyFunSpec with Matchers {
  describe("Number parser") {
    it("Should parse integers") {
      TestUtil.parse("100", ExpressionParser.expressionParser) shouldBe IntConst(100)
    }

    it("Should parse longs") {
      TestUtil.parse("100l", ExpressionParser.expressionParser) shouldBe LongConst(100)
      TestUtil.parse("100L", ExpressionParser.expressionParser) shouldBe LongConst(100)
    }

    it("Should parse floats") {
      TestUtil.parse("123.123f", ExpressionParser.expressionParser) shouldBe FloatConst(123.123)
      TestUtil.parse("123.123F", ExpressionParser.expressionParser) shouldBe FloatConst(123.123)
    }

    it("Should parse doubles") {
      TestUtil.parse("123.123", ExpressionParser.expressionParser) shouldBe DoubleConst(123.123)
    }
  }
}
