package transpiler.codegen

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import transpiler.utils.{CompileUtils, JavaScriptCompare}


class StatementCodeGenTest extends AnyFunSpec with Matchers {
  describe("Model parser") {
    it("Should parse a model with no fields") {
      val code =
        """package x.y.z
          |class ClassName {
          |
          |  let variable: Int = 1000
          |
          |  let x() Int ={
          |    let y = 10
          |    let array = []
          |    for i in array {
          |      if y < 5 {
          |        println("Something")
          |      } elif false {
          |        println("Else")
          |      } else {
          |        println(false)
          |      }
          |    }
          |  }
          |}
        """.stripMargin.replace("\r", "")

      val compiledCode: String = CompileUtils.compileCode(code)

      println(compiledCode)

      val expectedResult: String =
        """package x.y.z;
          |class ClassName {
          |    variable() {
          |        return 1000;
          |    }
          |    x() {
          |        var y = 10;
          |        var array = [];
          |        array.forEach(i => {
          |            if (y < 5) {
          |                console.log("Something");
          |            } else {
          |                if (false) {
          |                    console.log("Else");
          |                } else {
          |                    console.log(false);
          |                }
          |            }
          |        });
          |    }
          |}
        """.stripMargin.replace("\r", "")

      JavaScriptCompare.checkMatch(expectedResult, compiledCode)
    }

  }
}
