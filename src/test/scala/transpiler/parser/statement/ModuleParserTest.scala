package transpiler.parser.statement

import transpiler.parser.ast.AST._
import transpiler.parser.StatementParser
import transpiler.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class ModuleParserTest extends FunSpec with Matchers
{
  describe("ModuleIR parser")
  {
    it("Should parse modules")
    {
      val code ="package x.y.z\nclass ClassName\n  let x(): Int = 1\n"

      TestUtil.parse(code, StatementParser.moduleParser) shouldBe Module(ModuleHeader(NameSpace(ArrayBuffer(Name("x"), Name("y"), Name("z"))),ArrayBuffer()),ArrayBuffer(ClassModel(Name("ClassName"),List(),List(),None,List(),ArrayBuffer(),ArrayBuffer(Method(Name("x"),List(),ArrayBuffer(),ArrayBuffer(),Some(Type(RefLocal(Name("Int")))),Inline(IntConst(1)))))))
    }
  }
}
