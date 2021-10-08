package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._

import scala.collection.mutable.ArrayBuffer


class ReassignParserTest extends AnyFunSpec with Matchers {
  describe("Reassign parser") {
    it("Should parse reassignment an inline statementParser") {
      val code = "x = 2"
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe Reassign(Name("x"),ExprAsStmt(IntConst(2)))
    }
  }
}
