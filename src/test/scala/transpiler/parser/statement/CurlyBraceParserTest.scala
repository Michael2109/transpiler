package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._

import scala.collection.mutable.ArrayBuffer


class CurlyBraceParserTest extends AnyFunSpec with Matchers {
  describe("Brace block parser") {

    it("Should parse brace block inline") {
      val code = "{ x }"

      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x")))))
    }

    it("Should parse brace block inline - No space (left)") {
      val code = "{x }"

      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x")))))
    }

    it("Should parse brace block inline - No space (right)") {
      val code = "{ x}"

      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x")))))
    }


    it("Should parse brace block inline - No space (both)") {
      val code = "{x}"

      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x")))))
    }

    it("Should parse curly brace block") {
      val code =
        """{
          |  x
          |  y
          |  z
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y"))), ExprAsStmt(Identifier(Name("z")))))
    }

    it("Should parse curly brace block - No spaces (left)") {
      val code =
        """{
          |x
          |y
          |z
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y"))), ExprAsStmt(Identifier(Name("z")))))
    }


    it("Should parse curly brace block - Expression on first line") {
      val code =
        """{x
          |y
          |z
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y"))), ExprAsStmt(Identifier(Name("z")))))
    }


    it("Should parse curly brace block - Expression on last line") {
      val code =
        """{
          |x
          |y
          |z}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y"))), ExprAsStmt(Identifier(Name("z")))))
    }


    it("Should parse curly brace block - Expression on first and last line") {
      val code =
        """{x
          |y
          |z}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y"))), ExprAsStmt(Identifier(Name("z")))))
    }
  }
}
