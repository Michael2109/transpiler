package transpiler.codegen

import transpiler.js._;

object StatementCodeGen extends ExpressionCodeGen {

  def statementGenCode(statement: StatementJS): String = statement match {
    case identifierIR: IdentifierJS => identifierIR.name
    case doBlock: DoBlockJS => blockGenCode(doBlock.asInstanceOf[BlockJS])
    case forIR: ForJS => forLoopGenCode(forIR)
    case exprAsStmt: ExprAsStmtJS => expressionGenCode(exprAsStmt.expressionIR) + ";"
    case AddJS => "+"
    case SubJS => "-"
    case MulJS => "*"
    case DivJS => "/"
    case LessJS$ => "<"
    case LessEqualJS$ => "<="
    case GreaterJS$ => ">"
    case GreaterEqualJS$ => ">="
    case EqualJS$ => "=="
    case inline: InlineJS => blockGenCode(inline.asInstanceOf[BlockJS])
    case assignIR: AssignJS => assignGenCode(assignIR)
    case IfStatementJS(condition, isStmt, elseStmt) => {
      val sb = new StringBuilder()
      sb.append("if(")
      sb.append(expressionGenCode(condition))
      sb.append("){")
      sb.append(statementGenCode(isStmt))
      sb.append("}")
      if (elseStmt.isDefined) {
        sb.append("else{")
        sb.append(statementGenCode(elseStmt.get))
        sb.append("}")
      }
      sb.toString()
    }
  }

  def assignGenCode(assignIR: AssignJS): String = {
    s"var ${assignIR.name} = ${blockGenCode(assignIR.block)};"
  }

  def blockGenCode(block: BlockJS): String = {
    block match {
      case doBlock: DoBlockJS => doBlock.statements.map(statement => statementGenCode(statement)).mkString
      case inline: InlineJS => expressionGenCode(inline.expression)
    }
  }

  def fieldGenCode(field: FieldJS): String = {
    val sb = new StringBuilder()
    sb.append(s"${field.name}(){")
    if (field.init.isDefined) {
      sb.append("return ")
      sb.append(statementGenCode(field.init.get))
      sb.append("}")
    }

    sb.toString()
  }

  def forLoopGenCode(forIR: ForJS): String = {

    val variableName = forIR.identifierIR.name
    val sb = new StringBuilder()
    sb.append(expressionGenCode(forIR.expressionIR))
    sb.append(s".forEach($variableName => {")
    sb.append(blockGenCode(forIR.blockIR))
    sb.append("});")

    sb.toString()
  }

  def methodGenCode(method: MethodJS): String = {
    val sb = new StringBuilder()
    sb.append(method.name)
    sb.append("(")
    sb.append(method.fields.map(_._1).mkString(","))
    sb.append("){")
    sb.append(blockGenCode(method.body))
    sb.append("};")

    sb.toString()
  }

  def modelGenCode(model: ModelJS): String = {
    val fields = model.fields.map(field => fieldGenCode(field)).mkString
    val methods = model.methods.map(m => methodGenCode(m)).mkString

    s"class ${model.name} { ${fields} $methods};"
  }

}
