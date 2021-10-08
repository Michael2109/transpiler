package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.StatementParser._
import transpiler.parser.ast._

import scala.collection.mutable.ArrayBuffer

class ForLoopParserTest extends AnyFunSpec with Matchers {
  describe("For loop parser") {
    it("Should parse for loop - inline") {
      val code = "for x in y { x }"
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe For(Identifier(Name("x")),Identifier(Name("y")),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))))
    }
    it("Should parse for loop - block") {
      val code =
        """for x in y {
          |  x
          |}
          """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe For(Identifier(Name("x")),Identifier(Name("y")),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))))))
    }
  }
}
