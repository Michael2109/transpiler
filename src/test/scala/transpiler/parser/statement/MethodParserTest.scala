package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast._

import scala.collection.mutable.ArrayBuffer


class MethodParserTest extends AnyFunSpec with Matchers {
  describe("Method parser") {
    it("Should parse method definitions with no fields") {

      val Parsed.Success(value, _) = parse("let exampleMethod () Int = { a }", StatementParser.statementParser(_))
      value shouldBe Method(Name("exampleMethod"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("a"))))))
    }

    it("Should parse method definitions with multiple fields") {
      val Parsed.Success(value, _) = parse("let exampleMethod (a: Int, b: Int) Int = { a }", StatementParser.statementParser(_))
      value shouldBe Method(Name("exampleMethod"), List(), ArrayBuffer(Parameter(Name("a"), Type(RefLocal(Name("Int"))), None), Parameter(Name("b"), Type(RefLocal(Name("Int"))), None)), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("a"))))))
    }

    it("Should parse method definitions with a do block") {
      val code =
        """let exampleMethod() Int = {
          |  1
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe Method(Name("exampleMethod"), List(), ArrayBuffer(), List(), Some(Type(RefLocal(Name("Int")))), CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(IntConst(1)))))
    }

    it("Should parse method definitions with nested methods") {
      val code =
        """let outerMethod() Int = {
          |  let innerMethod() Int = {
          |    i
          |  }
          |  j
          | }
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe Method(Name("outerMethod"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), CurlyBracketsBlock(ArrayBuffer(Method(Name("innerMethod"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(Identifier(Name("i")))))), ExprAsStmt(Identifier(Name("j"))))))
    }

    it("Should parse method definitions with multiple statements") {
      val code =
        """let method() Int = {
          |  if true {
          |    1
          |  }
          |  else {
          |    2
          |  }
          |  let y = 10
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe Method(Name("method"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), CurlyBracketsBlock(ArrayBuffer(If(BoolConst(true), CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(IntConst(1)))), Some(CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(IntConst(2)))))), Assign(Name("y"), None, true, Inline(IntConst(10))))))
    }

    it("Should parse method definitions with method calls") {
      val code =
        """let method() Int = {
          |  let y = 1
          |  println(y)
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe Method(Name("method"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), CurlyBracketsBlock(ArrayBuffer(Assign(Name("y"), None, true, Inline(IntConst(1))), ExprAsStmt(MethodCall(Name("println"), ArrayBuffer(Identifier(Name("y"))))))))
    }

    it("Should parse method definitions with modifiers") {
      val code =
        """protected private abstract let method() Int = { 1 }
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.statementParser(_))
      value shouldBe Method(Name("method"), List(), ArrayBuffer(), ArrayBuffer(Protected, Private, Abstract), Some(Type(RefLocal(Name("Int")))), CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(IntConst(1)))))
    }
  }
}
