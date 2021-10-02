package transpiler.codegen

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import transpiler.codegen.CodeGen
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._
import transpiler.parser.ast.AST2IR
import transpiler.parser.ast.ModelIR
import transpiler.utils.TestUtil

class CodeGenTest extends AnyFunSpec with Matchers {
  describe("Model parser") {
    it("Should parse a model with no fields") {
      val code =
        """package x.y.z
          |class ClassName
          |  let x(): Int = do
          |    let y = 10
          |
        """.stripMargin.replace("\r", "")
      val ast: Module = TestUtil.parse(code, StatementParser.moduleParser).asInstanceOf[Module]

      // Process AST
      val modelIRs: Seq[ModelIR] = Array(ast).map(x => AST2IR.astToIR(x)).head

      println(ast)
      println(modelIRs)


      val compiledCode = modelIRs.map(CodeGen.genModelCode)
      println(compiledCode)
    }

  }
}
