package cobalt.parser.statement

import cobalt.ast.AST._
import cobalt.parser.StatementParser
import cobalt.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class IfStatementParserTest extends FunSpec with Matchers
{
    describe("If statement parser")
  {
    it("Should parse if statementParser - inline")
    {
      val code =
        """if true then x
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe If(Identifier(Name("true")),Inline(Identifier(Name("x"))),None)
    }

    it("Should parse if statementParser - do block")
    {
      val code =
        """if true then do
          |  x
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),None)
    }

    it("Should parse if statementParser - if else")
    {
      val code =
        """if true then do
          |  x
          |else do
          |  2
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),Some(DoBlock(ArrayBuffer(ExprAsStmt(IntConst(2))))))
    }

    it("Should parse if statementParser - elif")
    {
      val code =
        """if true then do
          |  x
          |elif true then do
          |  y
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),Some(If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("y"))))),None)))
    }

    it("Should parse if statementParser - multiple elif")
    {
      val code =
        """if true then do
          |  x
          |elif true then do
          |  y
          |elif true then do
          |  z
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),Some(If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("y"))))),Some(If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("z"))))),None)))))
    }

    it("Should parse if statementParser - elif else")
    {
      val code =
        """if true then do
          |  x
          |elif true then do
          |  y
          |else do
          |  z
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),Some(If(Identifier(Name("true")),DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("y"))))),Some(DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("z")))))))))
    }
  }

  /*
  let codeFalse = unlines [ "if(False) then"
  , "    x"
  ]
  */

  /*
      let codeElifTrue = unlines [ "if(True) then"
                               , "  i"
                               , "elif(True) then"
                               , "  j"
                               ]
   */

  /*
      let codeElifFalse = unlines [ "if(False) then"
                                , "  i"
                                , "elif(False) then"
                                , "  j"
                                ]
   */

  /*
      let codeElifElse = unlines [ "if(True) then"
                               , "  i"
                               , "elif(True) then"
                               , "  j"
                               , "else"
                               , "  k"
                               ]
   */

  /*
      let codeElse = unlines [ "if(True) then"
                           , "  i"
                           , "else"
                           , "  k"
                           ]
   */

  /*
      let codeMultipleElifsFinishedWithElse = unlines [ "if(True) then"
                                                    , "    x"
                                                    , "elif(True) then"
                                                    , "    i"
                                                    , "elif(False) then"
                                                    , "    f"
                                                    , "else"
                                                    , "    l"
                                                    ]
   */

  /*
      let codeMultipleElifsWithoutElse = unlines [ "if(True) then"
                                               , "    x"
                                               , "elif(True) then"
                                               , "    y"
                                               , "elif(False) then"
                                               , "    z"
                                               ]
   */

  /*
      let codeNestedWithoutElseNoParentheses = unlines [ "if (True) then"
                                                     , "    if (False) then "
                                                     , "        k"
                                                     , "    if True then"
                                                     , "        j"
                                                     , "    else"
                                                     , "        m"
                                                     ]
   */
}
