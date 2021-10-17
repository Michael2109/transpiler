package transpiler.utils

import fastparse.{Parsed, parse}
import transpiler.codegen.StatementCodeGen
import transpiler.js.{AST2JS, ModuleJS}
import transpiler.parser.StatementParser

object CompileUtils {

  def compileCode(code: String): String ={

    val Parsed.Success(ast, _) = parse(code, StatementParser.moduleParser(_))

    val moduleJS: ModuleJS = AST2JS.moduleToIR(ast)

    StatementCodeGen.moduleGenCode(moduleJS)
  }

}
