package transpiler.parser.statement

import transpiler.parser.ast.AST._
import transpiler.parser.{ExpressionParser, StatementParser}
import transpiler.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TernaryParserTest extends FunSpec with Matchers
{
    describe("Ternary parser") {
      it("Should parse ternary") {
        val code =
          """if true then x else y
          """.stripMargin.replace("\r", "")
        TestUtil.parse(code, ExpressionParser.expressionParser) shouldBe Ternary(Identifier(Name("true")),Identifier(Name("x")),Identifier(Name("y")))
      }
    }
}
