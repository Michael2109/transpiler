package transpiler.parser.expression

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.ast._

import transpiler.parser.StatementParser._
import scala.collection.mutable.ArrayBuffer

class NewClassInstanceParserTest extends AnyFunSpec with Matchers {
  describe("New class instance parser") {
    it("Should parse new class instances") {
      val Parsed.Success(value, _) = parse("new ClassName()", expressionParser(_))
     value shouldBe NewClassInstance(Type(RefLocal(Name("ClassName"))), ArrayBuffer(), None)
    }
  }
}
