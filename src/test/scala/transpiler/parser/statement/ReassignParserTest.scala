package transpiler.parser.statement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._


import scala.collection.mutable.ArrayBuffer


class ReassignParserTest extends AnyFunSpec with Matchers {
  describe("Reassign parser") {
    it("Should parse reassignment an inline statementParser") {
      // TestUtil.parse("x <- 2", StatementParser.statementParser) shouldBe Reassign(Name("x"), Inline(IntConst(2)))
    }

    it("Should parse reassignment with a do block") {
      val code =
        """x <- do
          |  1
          |  2
          |  3
        """.stripMargin.replace("\r", "")
      // TestUtil.parse(code, StatementParser.statementParser) shouldBe Reassign(Name("x"), CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(IntConst(1)), ExprAsStmt(IntConst(2)), ExprAsStmt(IntConst(3)))))
    }
  }
}
