package transpiler.parser.expression

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.ast.AST.{BoolConst, IntConst, Ternary}

class TupleParserTest extends AnyFunSpec with Matchers {
  describe("Tuple parser") {
    it("Should parse a tuple") {


     // val Parsed.Success(value, _) = parse("(1, 25, word)", ExpressionParser.expressionParser(_))
    //  value shouldBe Ternary(BoolConst(true),IntConst(1),IntConst(2))

    }
  }
}
