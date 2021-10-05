package transpiler.parser.statement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class PrintParserTest extends AnyFunSpec with Matchers {
  describe("Method parser") {
    it("Should parse method definitions with no fields") {
      // // TestUtil.parse("let exampleMethod (): Int = do\n if true 1 else 2", StatementParser.stmt) shouldBe Name(identifier("true"),Load)
    }
  }
}
