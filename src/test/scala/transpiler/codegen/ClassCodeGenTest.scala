package transpiler.codegen

import fastparse.{Parsed, parse}
import jdk.nashorn.internal.parser.Parser
import jdk.nashorn.internal.runtime.{Context, ErrorManager, Source}
import jdk.nashorn.internal.runtime.options.Options
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import transpiler.js.{AST2JS, ModelJS, ModuleJS}
import transpiler.parser.StatementParser
import transpiler.utils.{JavaScriptCompare, JavascriptBeautifier}


class ClassCodeGenTest extends AnyFunSpec with Matchers {
  describe("Model parser") {
    it("Should parse a model with no fields") {
      val code =
        """package a.b.c
          |class ClassName {
          |}
        """.stripMargin.replace("\r", "")

      val Parsed.Success(ast, _) = parse(code, StatementParser.moduleParser(_))

      val moduleJS: ModuleJS = AST2JS.moduleToIR(ast)

      val compiledCode: String = StatementCodeGen.moduleGenCode(moduleJS)

      val expectedResult: String =
      """package a.b.c;
        |class ClassName {
        |}
        """.stripMargin.replace("\r", "")

      JavaScriptCompare.checkMatch(expectedResult, compiledCode)
    }

  }
}
