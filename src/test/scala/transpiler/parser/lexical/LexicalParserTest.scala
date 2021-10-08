package transpiler.parser.lexical

import fastparse._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.{ExpressionParser}
import transpiler.parser.ast._
import transpiler.parser.StatementParser._

class LexicalParserTest extends AnyFunSpec with Matchers {
  describe("Lexical parser") {
    it("Should parse integers") {
      val Parsed.Success(value, _) = parse("1",  expressionParser(_))
      value shouldBe IntConst(1)
    }
  }
}
