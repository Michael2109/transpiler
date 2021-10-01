package transpiler.parser.statement

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._
import transpiler.utils.TestUtil

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class ReassignParserTest extends FunSpec with Matchers {
  describe("Reassign parser") {
    it("Should parse reassignment an inline statementParser") {
      TestUtil.parse("x <- 2", StatementParser.statementParser) shouldBe Reassign(Name("x"), Inline(IntConst(2)))
    }

    it("Should parse reassignment with a do block") {
      val code =
        """x <- do
          |  1
          |  2
          |  3
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe Reassign(Name("x"), DoBlock(ArrayBuffer(ExprAsStmt(IntConst(1)), ExprAsStmt(IntConst(2)), ExprAsStmt(IntConst(3)))))
    }
  }
}
