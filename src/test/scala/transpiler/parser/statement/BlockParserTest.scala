package transpiler.parser.statement

import fastparse.Parsed.Failure
import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.{ExpressionParser, StatementParser}
import transpiler.parser.ast.AST._

import scala.collection.mutable.ArrayBuffer


class BlockParserTest extends AnyFunSpec with Matchers {
  describe("Brace block parser") {

    it("Should parse brace block inline") {
      val code = "{ x }"

      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_))
      value shouldBe BraceBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x")))))
    }

    it("Should parse brace block") {
      val code =
        """{
          |  x
          |  y
          |  z
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.curlyBracketsBlock(_), true)
      value shouldBe BraceBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y"))), ExprAsStmt(Identifier(Name("z")))))
    }
  }
}
