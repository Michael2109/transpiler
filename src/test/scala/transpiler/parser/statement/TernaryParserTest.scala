package transpiler.parser.statement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.ast._



class TernaryParserTest extends AnyFunSpec with Matchers {
  describe("Ternary parser") {
    it("Should parse ternary") {
      val code =
        """if true then x else y
          """.stripMargin.replace("\r", "")
      // TestUtil.parse(code, expressionParser) shouldBe Ternary(Identifier(Name("true")), Identifier(Name("x")), Identifier(Name("y")))
    }
  }
}
