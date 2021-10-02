package transpiler.parser.expression

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.ast.AST._
import transpiler.utils.TestUtil

import scala.collection.mutable.ArrayBuffer

class MethodCallParserTest extends AnyFunSpec with Matchers {

  describe("Method call parser test") {
    it("Should parse method calls - No arguments") {
      TestUtil.parse("println()", ExpressionParser.expressionParser) shouldBe MethodCall(Name("println"), ArrayBuffer(BlockExpr(List())))
    }
    it("Should parse method calls - Single argument") {
      TestUtil.parse("methodCall(a)", ExpressionParser.expressionParser) shouldBe MethodCall(Name("methodCall"), ArrayBuffer(Identifier(Name("a"))))
    }
    it("Should parse method calls - Multiple arguments") {
      TestUtil.parse("methodCall(a, b, c)", ExpressionParser.expressionParser) shouldBe MethodCall(Name("methodCall"), ArrayBuffer(Identifier(Name("a")), Identifier(Name("b")), Identifier(Name("c"))))
    }

    it("Should parse method calls - New class instance") {
      TestUtil.parse("methodCall(new ClassName())", ExpressionParser.expressionParser) shouldBe MethodCall(Name("methodCall"), ArrayBuffer(NewClassInstance(Type(RefLocal(Name("ClassName"))), ArrayBuffer(BlockExpr(List())), None)))
    }
  }
}
