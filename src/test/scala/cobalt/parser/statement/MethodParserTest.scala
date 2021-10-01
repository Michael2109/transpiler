package cobalt.parser.statement

import cobalt.ast.AST._
import cobalt.parser.StatementParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class MethodParserTest extends FunSpec with Matchers
{
  describe("Method parser")
  {
    it("Should parse method definitions with no fields")
    {
      TestUtil.parse("let exampleMethod (): Int = _", StatementParser.statementParser) shouldBe Method(Name("exampleMethod"),List(),ArrayBuffer(),List(),Some(Type(RefLocal(Name("Int")))),Inline(Identifier(Name("_"))))
    }

    it("Should parse method definitions with multiple fields")
    {
      TestUtil.parse("let exampleMethod (a: Int, b: Int): Int = _", StatementParser.statementParser) shouldBe Method(Name("exampleMethod"),List(),ArrayBuffer(Field(Name("a"),Type(RefLocal(Name("Int"))),None), Field(Name("b"),Type(RefLocal(Name("Int"))),None)),List(),Some(Type(RefLocal(Name("Int")))),Inline(Identifier(Name("_"))))
    }

    it("Should parse method definitions with a do block")
    {
      val code =
        """let exampleMethod(): Int = do
          |  1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe Method(Name("exampleMethod"),List(),ArrayBuffer(),List(),Some(Type(RefLocal(Name("Int")))),DoBlock(ArrayBuffer(ExprAsStmt(IntConst(1)))))
    }

    it("Should parse method definitions with nested methods")
    {
      val code =
        """let outerMethod(): Int = do
          |  let innerMethod(): Int = do
          |    i
          |  j
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe Method(Name("outerMethod"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),DoBlock(ArrayBuffer(Method(Name("innerMethod"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("i")))))), ExprAsStmt(Identifier(Name("j"))))))
    }

    it("Should parse method definitions with multiple statements")
    {
      val code =
        """let method(): Int = do
          |  if true then do
          |    1
          |  else
          |    2
          |  let y = 10
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe Method(Name("method"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),DoBlock(ArrayBuffer(If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(IntConst(1)))),Some(Inline(IntConst(2)))), Assign(Name("y"),None,true,Inline(IntConst(10))))))
    }

    it("Should parse method definitions with method calls")
    {
      val code =
        """let method(): Int = do
          |  let y = 1
          |  println(y)
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe Method(Name("method"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),DoBlock(ArrayBuffer(Assign(Name("y"),None,true,Inline(IntConst(1))), ExprAsStmt(MethodCall(Name("println"),ArrayBuffer(Identifier(Name("y"))))))))
    }

    it("Should parse method definitions with modifiers")
    {
      val code =
        """protected private abstract let method(): Int = 1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe Method(Name("method"),List(),ArrayBuffer(),ArrayBuffer(Protected(), Private(), Abstract()),Some(Type(RefLocal(Name("Int")))),Inline(IntConst(1)))
    }
  }
}
