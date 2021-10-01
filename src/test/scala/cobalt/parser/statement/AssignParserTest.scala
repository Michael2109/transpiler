package cobalt.parser.statement

import cobalt.ast.AST._
import cobalt.parser.StatementParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class AssignParserTest extends FunSpec with Matchers
{
  describe("Assignment parser")
  {
    it("Should parse assignment")
    {
      TestUtil.parse("let x = 10", StatementParser.statementParser) shouldBe Assign(Name("x"),None,true,Inline(IntConst(10)))
    }

    it("Should parse mutable assignment")
    {
      TestUtil.parse("let mutable x = 10", StatementParser.statementParser) shouldBe Assign(Name("x"),None,false,Inline(IntConst(10)))
    }

    it("Should parse with type defined")
    {
      TestUtil.parse("let x: Int = 10", StatementParser.statementParser) shouldBe Assign(Name("x"),Some(Type(RefLocal(Name("Int")))),true,Inline(IntConst(10)))
    }

    it("Should parse with a do block")
    {
      val code =
        """let x = do
          |  1
          |  2
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe Assign(Name("x"),None,true,DoBlock(ArrayBuffer(ExprAsStmt(IntConst(1)), ExprAsStmt(IntConst(2)))))
    }
  }

  describe("Multiple assignment parser") {
    // TODO Assign parser multiple

    // TODO "let x,y = z"

    // TODO "let mutable x,y = z"

    // TODO "let x,y: Int = z"

    // TODO "let x,y = do"
    //      "    i"
    //      "    j"

    // TODO "let x,y: Int = do"
    //      "    i"
    //      "    j"
  }
}
