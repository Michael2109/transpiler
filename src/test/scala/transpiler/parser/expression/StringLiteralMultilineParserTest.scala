package transpiler.parser.expression

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers

class StringLiteralMultilineParserTest extends AnyFunSpec with Matchers {
  describe("Method parser") {
    it("Should parse method definitions with no fields") {
      // TestUtil.parse("let exampleMethod (): Int = do\n if true 1 else 2", StatementParser.stmt) shouldBe Name(identifier("true"),Load)
    }
  }
}
