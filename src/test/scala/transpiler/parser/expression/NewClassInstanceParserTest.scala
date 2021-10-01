package transpiler.parser.expression

import transpiler.parser.ast.AST._
import transpiler.parser.ExpressionParser
import transpiler.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class NewClassInstanceParserTest extends FunSpec with Matchers
{
  describe("New class instance parser")
  {
    it("Should parse new class instances")
    {
      TestUtil.parse("new ClassName()", ExpressionParser.expressionParser) shouldBe NewClassInstance(Type(RefLocal(Name("ClassName"))),ArrayBuffer(BlockExpr(List())),None)
    }
  }
}
