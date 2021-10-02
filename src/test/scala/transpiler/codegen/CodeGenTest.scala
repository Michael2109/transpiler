package transpiler.codegen

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}
import transpiler.code_gen.CodeGen
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST._
import transpiler.parser.ast.AST2IR
import transpiler.parser.ast.IRNew.ModelIR
import transpiler.utils.TestUtil

@RunWith(classOf[JUnitRunner])
class CodeGenTest extends FunSpec with Matchers {
  describe("Model parser") {
    it("Should parse a model with no fields") {
      val code =
        """package x.y.z
          |class ClassName
          |  let x(): Int = 1
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
