package transpiler.parser.expression

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser._
import transpiler.parser.ast._

import scala.collection.mutable.ArrayBuffer

class MethodCallParserTest extends AnyFunSpec with Matchers {

  describe("Method call parser test") {
    it("Should parse method calls - No arguments") {
      val Parsed.Success(value, _) = parse("println()", expressionParser(_))
      value shouldBe MethodCall(Name("println"), ArrayBuffer())
    }
    it("Should parse method calls - Single argument") {
      val Parsed.Success(value, _) = parse("methodCall(a)", methodCallParser(_))
      value shouldBe MethodCall(Name("methodCall"), ArrayBuffer(Identifier(Name("a"))))
    }
    it("Should parse method calls - Multiple arguments") {
      val Parsed.Success(value, _) = parse("methodCall(a, b, c)", expressionParser(_))
      value shouldBe MethodCall(Name("methodCall"), ArrayBuffer(Identifier(Name("a")), Identifier(Name("b")), Identifier(Name("c"))))
    }

    it("Should parse method calls - New class instance") {
      val Parsed.Success(value, _) = parse("methodCall(new ClassName())", expressionParser(_))
      value shouldBe MethodCall(Name("methodCall"), ArrayBuffer(NewClassInstance(Type(RefLocal(Name("ClassName"))), ArrayBuffer(), None)))
    }
  }
}
