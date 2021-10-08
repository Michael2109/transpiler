package transpiler.parser.expression

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.StatementParser._
import transpiler.parser.ast._
class TernaryParserTest extends AnyFunSpec with Matchers {
  describe("Ternary parser") {
    it("Should parse ternary") {

      // todo include ternary
      val Parsed.Success(value, _) = parse("true ? 1 : 2", ternaryParser(_))
      value shouldBe Ternary(BoolConst(true),IntConst(1),IntConst(2))
    }
  }
}
