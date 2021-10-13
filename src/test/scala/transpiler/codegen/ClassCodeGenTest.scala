package transpiler.codegen

import fastparse.{Parsed, parse}
import jdk.nashorn.internal.parser.Parser
import jdk.nashorn.internal.runtime.{Context, ErrorManager, Source}
import jdk.nashorn.internal.runtime.options.Options
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import transpiler.js.{AST2JS, ModelJS, ModuleJS}
import transpiler.parser.StatementParser
import transpiler.utils.JavascriptBeautifier


class ClassCodeGenTest extends AnyFunSpec with Matchers {
  describe("Model parser") {
    it("Should parse a model with no fields") {
      val code =
        """package a.b.c
          |class ClassName {
          |}
        """.stripMargin.replace("\r", "")

      val Parsed.Success(ast, _) = parse(code, StatementParser.moduleParser(_))

      println(ast)
      // Process AST
      val moduleJS: ModuleJS = AST2JS.moduleToIR(ast)

      println(moduleJS)

      val compiledCode: String = StatementCodeGen.moduleGenCode(moduleJS)
      println(compiledCode.head)

      val options = new Options("nashorn");
      options.set("anon.functions", true);
      options.set("parse.only", true);
      options.set("scripting", true);
      options.set("language", "es6")

      val errors = new ErrorManager();
      val context = new Context(options, errors, Thread.currentThread().getContextClassLoader());
      val source   =  Source.sourceFor("test", compiledCode);
      val parser = new Parser(context.getEnv(), source, errors);
      val functionNode = parser.parse();
      val block = functionNode.getBody();
      val statements = block.getStatements();

      println(JavascriptBeautifier.beautify(compiledCode))

      val expectedResult: String =
      """package a.b.c;
        |class ClassName {
        |}
        """.stripMargin.replace("\r", "")

      assertResult(JavascriptBeautifier.beautify(expectedResult))(JavascriptBeautifier.beautify(compiledCode))
    }

  }
}
