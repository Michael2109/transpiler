package transpiler.codegen.classes

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import transpiler.utils.{CompileUtils, JavaScriptCompare}


class ClassMethodCodeGenTest extends AnyFunSpec with Matchers {
  describe("Model parser") {
    it("Should compile a class with 1 method") {
      val code =
        """package a.b.c
          |class ClassName {
          |
          |  let method1() Unit= {
          |
          |  }
          |}
        """.stripMargin.replace("\r", "")

      val compiledCode: String = CompileUtils.compileCode(code)

      val expectedResult: String =
        """package a.b.c;
          |class ClassName {
          |  method1(){
          |  }
          |}
        """.stripMargin.replace("\r", "")

      JavaScriptCompare.checkMatch(expectedResult, compiledCode)
    }

    it("Should compile a class with multiple methods") {
      val code =
        """package a.b.c
          |class ClassName {
          |
          |  let method1() Unit= {
          |
          |  }
          |  let method2() Unit= {
          |
          |  }
          |  let method3() Unit= {
          |
          |  }
          |}
        """.stripMargin.replace("\r", "")

      val compiledCode: String = CompileUtils.compileCode(code)

      val expectedResult: String =
        """package a.b.c;
          |class ClassName {
          |  method1(){
          |  }
          |  method2(){
          |  }
          |  method3(){
          |  }
          |}
        """.stripMargin.replace("\r", "")

      JavaScriptCompare.checkMatch(expectedResult, compiledCode)
    }


  }
}
