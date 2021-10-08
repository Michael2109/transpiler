package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast._
import transpiler.parser.ast._
import scala.collection.mutable.ArrayBuffer


class AssignParserTest extends AnyFunSpec with Matchers {
  describe("Assignment parser") {
    it("Should parse assignment") {
      val Parsed.Success(value, _) = parse("let x = 10",  StatementParser.statementParser(_))
     value shouldBe Assign(Name("x"), None, true, Inline(IntConst(10)))
    }

    it("Should parse mutable assignment") {
      val Parsed.Success(value, _) = parse("let mutable x = 10",  StatementParser.statementParser(_))
   value shouldBe Assign(Name("x"), None, false, Inline(IntConst(10)))
    }

    it("Should parse with type defined") {
      val Parsed.Success(value, _) = parse("let x: Int = 10",  StatementParser.statementParser(_))
     value shouldBe Assign(Name("x"), Some(Type(RefLocal(Name("Int")))), true, Inline(IntConst(10)))
    }

    it("Should parse with a do block") {
      val code =
        """let x = do
          |  1
          |  2
        """.stripMargin.replace("\r", "")
      // TestUtil.parse(code, StatementParser.statementParser) shouldBe Assign(Name("x"), None, true, CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(IntConst(1)), ExprAsStmt(IntConst(2)))))
    }
  }

  describe("Multiple assignment parser") {
    // TODO Assign parser multiple

    // TODO "let x,y = z"

    // TODO "let mutable x,y = z"

    // TODO "let x,y: Int = z"

    // TODO "let x,y = do"
    //      "    i"
    //      "    j"

    // TODO "let x,y: Int = do"
    //      "    i"
    //      "    j"
  }
}
