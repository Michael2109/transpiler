package transpiler.codegen

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import transpiler.parser.StatementParser
import transpiler.parser.ast.{AST2IR, ModelIR}

class CodeGenTest extends AnyFunSpec with Matchers {
  describe("Model parser") {
    it("Should parse a model with no fields") {
      val code =
        """package x.y.zS
          |class ClassName {
          |  let x() Int ={
          |    let y = 10
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

      val compiledCode = modelIRs.map(CodeGen.genModelCode)
      println(compiledCode)


    }

  }
}
