package transpiler.parser.expression

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.StatementParser.expressionParser
import transpiler.parser.ast.{Identifier, MethodCall, Name, NestedExpr}

import scala.collection.mutable.ArrayBuffer

class ExpressionParserTest extends AnyFunSpec with Matchers {
  describe("Nested expression call parser test") {
    it("Should parse nested expression calls") {
      val Parsed.Success(value, _) = parse("x.toString()", expressionParser(_))
      value shouldBe NestedExpr(ArrayBuffer(Identifier(Name("x")), MethodCall(Name("toString"), ArrayBuffer())))
    }
  }

  // TODO "methodCall1().methodCall2()"

  // TODO "methodCall1(a).methodCall2(a)"

  // TODO "methodCall1(a, b, c).methodCall2(a, b, c)"

  // TODO "varName1.varName2"

  // TODO "methodCall1().varName1"

  // TODO "methodCall1(a).varName1"

  // TODO "methodCall1(a, b, c).varName1"

  // TODO "this.varName2"

  // TODO "super.varName2"
}
