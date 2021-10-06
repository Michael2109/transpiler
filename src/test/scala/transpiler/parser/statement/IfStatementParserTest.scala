package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._

import scala.collection.mutable.ArrayBuffer


class IfStatementParserTest extends AnyFunSpec with Matchers {
  describe("If statement parser") {
    it("Should parse if statement - inline (if)") {
      val code =        "if true { x }"
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
     value shouldBe If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),None)
    }

    it("Should parse if statement - inline (if, elif, else)") {
      val code = "if true { x } elif false { y } else { z }"
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),Some(If(BoolConst(false),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("y"))))),Some(CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("z")))))))))
    }

    it("Should parse if statement - inline (if, else)") {
      val code =        "if true { x } else { y }"
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),Some(CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("y")))))))
    }

    it("Should parse if statementParser - Block (if)") {
      val code =
        """if true {
          |  x
          |}
          """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))

     value  shouldBe If(BoolConst(true), CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))), None)
    }

    it("Should parse if statementParser - if else") {
      val code =
        """if true {
          |  x
          |}
          |else {
          |  2
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value  shouldBe If(BoolConst(true), CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))), Some(CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(IntConst(2))))))
    }

    it("Should parse if statementParser - elif") {
      val code =
        """if true {
          |  x
          |}
          |elif true {
          |  y
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value  shouldBe If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),Some(If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("y"))))),None)))
    }

    it("Should parse if statementParser - multiple elif") {
      val code =
        """if true {
          |  x
          |}
          |elif true {
          |  y
          |}
          |elif true {
          |  z
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value  shouldBe If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),Some(If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("y"))))),Some(If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("z"))))),None)))))
    }

    it("Should parse if statementParser - elif else") {
      val code =
        """if true {
          |  x
          |}
          |elif true {
          |  y
          |}
          |else {
          |  z
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
     value shouldBe If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))),Some(If(BoolConst(true),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("y"))))),Some(CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("z")))))))))
    }
  }

}
