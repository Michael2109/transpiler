package transpiler.parser.expression

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.ast.AST._
import transpiler.utils.TestUtil

import scala.collection.mutable.ArrayBuffer

class NewClassInstanceParserTest extends AnyFunSpec with Matchers {
  describe("New class instance parser") {
    it("Should parse new class instances") {
      TestUtil.parse("new ClassName()", ExpressionParser.expressionParser) shouldBe NewClassInstance(Type(RefLocal(Name("ClassName"))), ArrayBuffer(BlockExpr(List())), None)
    }
  }
}
