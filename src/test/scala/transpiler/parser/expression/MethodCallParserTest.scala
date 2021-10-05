package transpiler.parser.expression

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.ast.AST._

import scala.collection.mutable.ArrayBuffer

class MethodCallParserTest extends AnyFunSpec with Matchers {

  describe("Method call parser test") {
    it("Should parse method calls - No arguments") {
      val Parsed.Success(value, _) = parse("println()", ExpressionParser.expressionParser(_))
      value shouldBe MethodCall(Name("println"), ArrayBuffer(BlockExpr(List())))
    }
    it("Should parse method calls - Single argument") {
      val Parsed.Success(value, _) = parse("methodCall(a)", ExpressionParser.methodCallParser(_))
      value shouldBe MethodCall(Name("methodCall"), ArrayBuffer(Identifier(Name("a"))))
    }
    it("Should parse method calls - Multiple arguments") {
      val Parsed.Success(value, _) = parse("methodCall(a, b, c)", ExpressionParser.expressionParser(_))
      value shouldBe MethodCall(Name("methodCall"), ArrayBuffer(Identifier(Name("a")), Identifier(Name("b")), Identifier(Name("c"))))
    }

    it("Should parse method calls - New class instance") {
      val Parsed.Success(value, _) = parse("methodCall(new ClassName())", ExpressionParser.expressionParser(_))
      value shouldBe MethodCall(Name("methodCall"), ArrayBuffer(NewClassInstance(Type(RefLocal(Name("ClassName"))), ArrayBuffer(BlockExpr(List())), None)))
    }
  }
}
