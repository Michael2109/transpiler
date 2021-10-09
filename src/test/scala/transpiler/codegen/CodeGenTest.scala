package transpiler.codegen

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import transpiler.parser.StatementParser
import transpiler.parser.ast.{AST2IR, ModelIR}

import jdk.nashorn.internal.parser.Parser
import jdk.nashorn.internal.runtime.Context
import jdk.nashorn.internal.runtime.ErrorManager
import jdk.nashorn.internal.runtime.options.Options

import jdk.nashorn.internal.runtime.Source


class CodeGenTest extends AnyFunSpec with Matchers {
  describe("Model parser") {
    it("Should parse a model with no fields") {
      val code =
        """package x.y.zS
          |class ClassName {
          |  let x() Int ={
          |    let y = 10
          |    let array = []
          |    for i in array {
          |      if y < 5 {
          |        println("Something")
          |      }
          |    }
          |  }
          |}
        """.stripMargin.replace("\r", "")

      val Parsed.Success(ast, _) = parse(code, StatementParser.fileParser(_))

      println(ast)
      // Process AST
      val modelIRs: Seq[ModelIR] = Array(ast).map(x => AST2IR.astToIR(x)).head

      println(modelIRs)

      val compiledCode: List[String] = modelIRs.map(CodeGen.genModelCode).toList
      println(compiledCode.head)

      val options = new Options("nashorn");
      options.set("anon.functions", true);
      options.set("parse.only", true);
      options.set("scripting", true);
      options.set("language", "es6")

      val errors = new ErrorManager();
      val context = new Context(options, errors, Thread.currentThread().getContextClassLoader());
      val source   =  Source.sourceFor("test", compiledCode.head);
      val parser = new Parser(context.getEnv(), source, errors);
      val functionNode = parser.parse();
      val block = functionNode.getBody();
      val statements = block.getStatements();

      println(block)
      println(statements)
    }

  }
}
