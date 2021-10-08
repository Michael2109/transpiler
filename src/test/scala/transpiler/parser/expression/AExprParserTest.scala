package transpiler.parser.expression

import fastparse._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.StatementParser.expressionParser
import transpiler.parser.ast._

class AExprParserTest extends AnyFunSpec with Matchers {
  describe("Number parsers") {

    it("Should parse integers") {

      val Parsed.Success(value, _) = parse("100", expressionParser(_))
      value shouldBe IntConst(100)
    }
    it("Should parse negative") {
     //todo  val Parsed.Success(value, _) = parse("-1", expressionParser(_))
      //value shouldBe IntConst(-100)
    }
  }

  describe("Should parse floats") {

    it("Should parse floats") {
      val Parsed.Success(value, _) = parse("123.123f", expressionParser(_))
      value shouldBe FloatConst(123.123)

    }

  }

  describe("Should parse doubles") {

    it("Should parse doubles") {
      val Parsed.Success(value, _) = parse("123.123", expressionParser(_))
      value shouldBe DoubleConst(123.123)
    }
  }

  describe("Should parse longs") {

    it("Should parse longs") {
      val Parsed.Success(value, _) = parse("100L", expressionParser(_))
      value shouldBe LongConst(100)
    }

  }
  describe("Arithmetic parsers") {
    it("Should parse addition") {
      val Parsed.Success(value, _) = parse("1+2", expressionParser(_))
      value shouldBe ABinary(Add, IntConst(1), IntConst(2))
    }
    it("Should parse subtract") {
      val Parsed.Success(value, _) = parse("1-2", expressionParser(_))
      value shouldBe ABinary(Subtract, IntConst(1), IntConst(2))
    }
    it("Should parse multiply") {
      val Parsed.Success(value, _) = parse("1 * 2", expressionParser(_))
      value shouldBe ABinary(Multiply, IntConst(1), IntConst(2))
    }
    it("Should parse divide") {
      val Parsed.Success(value, _) = parse("1 / 2", expressionParser(_))
      value shouldBe ABinary(Divide, IntConst(1), IntConst(2))
    }
    it("Should parse mixed") {
      val Parsed.Success(value, _) = parse("1 / 100 * 3 + 200 - 4", expressionParser(_))
      value shouldBe ABinary(Subtract, ABinary(Add, ABinary(Multiply, ABinary(Divide, IntConst(1), IntConst(100)), IntConst(3)), IntConst(200)), IntConst(4))
    }
    it("Should parse parentheses - 1") {
      val Parsed.Success(value, _) = parse("1 / 100 * (2 + 200) - 3", expressionParser(_))
      value shouldBe ABinary(Subtract, ABinary(Multiply, ABinary(Divide, IntConst(1), IntConst(100)), ABinary(Add, IntConst(2), IntConst(200))), IntConst(3))
    }
  }


}
