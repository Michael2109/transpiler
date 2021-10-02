package transpiler.parser.statement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._
import transpiler.utils.TestUtil

import scala.collection.mutable.ArrayBuffer


class ModuleParserTest extends AnyFunSpec with Matchers {
  describe("ModuleIR parser") {
    it("Should parse modules") {
      val code = "package x.y.z\nclass ClassName\n  let x(): Int = 1\n"

      TestUtil.parse(code, StatementParser.moduleParser) shouldBe Module(ModuleHeader(NameSpace(ArrayBuffer(Name("x"), Name("y"), Name("z"))), ArrayBuffer()), ArrayBuffer(ClassModel(Name("ClassName"), List(), List(), None, List(), ArrayBuffer(), ArrayBuffer(Method(Name("x"), List(), ArrayBuffer(), ArrayBuffer(), Some(Type(RefLocal(Name("Int")))), Inline(IntConst(1)))))))
    }
  }
}
