package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast._

import scala.collection.mutable.ArrayBuffer


class ModuleParserTest extends AnyFunSpec with Matchers {
  describe("Module parser") {

    it("Should parse a module - No imports") {
      val code =
        """package a.b.c
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.fileParser(_))
      value shouldBe Module(ModuleHeader(Package(ArrayBuffer(Name("a"), Name("b"), Name("c"))),ArrayBuffer()),ArrayBuffer())
    }

    it("Should parse a module - No classes") {
      val code =
        """package a.b.c
          |
          |import x.y.z
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.fileParser(_))
      value shouldBe Module(ModuleHeader(Package(ArrayBuffer(Name("a"), Name("b"), Name("c"))),ArrayBuffer(Import(ArrayBuffer(Name("x"), Name("y"), Name("z"))))),ArrayBuffer())
    }

    it("Should parse a module - One class") {
      val code =
        """package a.b.c
          |
          |import x.y.z
          |
          |class A {
          |
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.fileParser(_))
      value shouldBe Module(ModuleHeader(Package(ArrayBuffer(Name("a"), Name("b"), Name("c"))),ArrayBuffer(Import(ArrayBuffer(Name("x"), Name("y"), Name("z"))))),ArrayBuffer(Model(ClassModelType,Name("A"),List(),List(),None,List(),ArrayBuffer(),ArrayBuffer())))
    }

    it("Should parse a module - Multiple classes") {
      val code =
        """package a.b.c
          |
          |import x.y.z
          |
          |class A {}
          |class B {}
          |class C {}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.fileParser(_))
      value shouldBe Module(ModuleHeader(Package(ArrayBuffer(Name("a"), Name("b"), Name("c"))),ArrayBuffer(Import(ArrayBuffer(Name("x"), Name("y"), Name("z"))))),ArrayBuffer(Model(ClassModelType,Name("A"),List(),List(),None,List(),ArrayBuffer(),ArrayBuffer()),Model(ClassModelType,Name("B"),List(),List(),None,List(),ArrayBuffer(),ArrayBuffer()),Model(ClassModelType,Name("C"),List(),List(),None,List(),ArrayBuffer(),ArrayBuffer())))
    }

    it("Should parse a module - class with nested content") {
      val code =
        """package a.b.c
          |
          |import x.y.z
          |
          |class Test {
          |  let x = 10
          |  let exampleMethod() Int = {
          |    1
          |  }
          |}
        """.stripMargin.replace("\r", "")
      val Parsed.Success(value, _) = parse(code, StatementParser.fileParser(_))
      value shouldBe Module(ModuleHeader(Package(ArrayBuffer(Name("a"), Name("b"), Name("c"))),ArrayBuffer(Import(ArrayBuffer(Name("x"), Name("y"), Name("z"))))),ArrayBuffer(Model(ClassModelType,Name("Test"),List(),List(),None,List(),ArrayBuffer(),ArrayBuffer(Assign(Name("x"),None,true,Inline(IntConst(10))), Method(Name("exampleMethod"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),CurlyBracketsBlock(ArrayBuffer(ExprAsStmt(IntConst(1)))))))))
    }
  }
}
