package transpiler.parser.statement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._
import transpiler.utils.TestUtil

import scala.collection.mutable.ArrayBuffer


class ModelParserTest extends AnyFunSpec with Matchers {
  describe("Model parser") {
    it("Should parse a model with no fields") {
      val code =
        """class Test
          |  let x = 10
          |  let exampleMethod(): Int = do
          |    1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe ClassModel(Name("Test"), List(), List(), None, List(), ArrayBuffer(), ArrayBuffer(Assign(Name("x"), None, true, Inline(IntConst(10))), Method(Name("exampleMethod"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), DoBlock(ArrayBuffer(ExprAsStmt(IntConst(1)))))))
    }

    it("Should parse a model that extends a parent") {
      val code =
        """class Test extends ParentClass
          |  let exampleMethod(): Int = 1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe ClassModel(Name("Test"), List(), List(), Some(Type(RefLocal(Name("ParentClass")))), List(), ArrayBuffer(), ArrayBuffer(Method(Name("exampleMethod"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), Inline(IntConst(1)))))
    }

    it("Should parse a model that extends a parent and implements a trait") {
      val code =
        """class Test extends ParentClass with Trait
          |  let exampleMethod(): Int = 1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe ClassModel(Name("Test"), List(), List(), Some(Type(RefLocal(Name("ParentClass")))), List(), ArrayBuffer(Type(RefLocal(Name("Trait")))), ArrayBuffer(Method(Name("exampleMethod"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), Inline(IntConst(1)))))
    }

    it("Should parse a model that extends a parent and implements multiple traits") {
      val code =
        """class Test extends ParentClass with Trait1 with Trait2 with Trait3
          |  let exampleMethod(): Int = 1
        """.stripMargin.replace("\r", "")
      TestUtil.parse(code, StatementParser.statementParser) shouldBe ClassModel(Name("Test"), List(), List(), Some(Type(RefLocal(Name("ParentClass")))), List(), ArrayBuffer(Type(RefLocal(Name("Trait1"))), Type(RefLocal(Name("Trait2"))), Type(RefLocal(Name("Trait3")))), ArrayBuffer(Method(Name("exampleMethod"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), Inline(IntConst(1)))))
    }
  }
}
