package transpiler.parser.statement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._
import transpiler.utils.TestUtil

import scala.collection.mutable.ArrayBuffer


class DoBlockParserTest extends AnyFunSpec with Matchers {
  describe("Do block parser") {
    it("Should parse do block") {
      val code =
        """do
          |  x
          |  y
          |  z
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.doBlock) shouldBe DoBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("x"))), ExprAsStmt(Identifier(Name("y"))), ExprAsStmt(Identifier(Name("z")))))
    }
  }
}
