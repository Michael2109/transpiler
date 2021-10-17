package transpiler.codegen.classes

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import transpiler.utils.{CompileUtils, JavaScriptCompare}


class ClassCodeGenTest extends AnyFunSpec with Matchers {
  describe("Model parser") {
    it("Should parse a class with no fields") {
      val code =
        """package a.b.c
          |class ClassName {
          |}
        """.stripMargin.replace("\r", "")

      val compiledCode: String = CompileUtils.compileCode(code)

      val expectedResult: String =
        """package a.b.c;
          |class ClassName {
          |}
        """.stripMargin.replace("\r", "")

      JavaScriptCompare.checkMatch(expectedResult, compiledCode)
    }

  }
}
