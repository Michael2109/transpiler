package transpiler.codegen

import transpiler.parser.ast._;

object CodeGen {

  def genModelCode(model: ModelIR): String = {

    val sb: StringBuilder = new StringBuilder()

    model match {
      case classModel: ClassModelIR => {

        sb.append(s"class ${classModel.name} {")

        //  classModel.externalStatements.foreach(v => genCode(stringBuilder, v))
        sb.append(classModel.methods.map(m => genCode(m)).mkString)

        sb.append("};")
      }
    }

    sb.toString()
  }

  def genCode(method: MethodIR): String = {
    val sb = new StringBuilder()
    sb.append(method.name)
    sb.append("(")
    sb.append(method.fields.map(_._1).mkString(","))
    sb.append("){")
    sb.append(genCode(method.body, method))
    sb.append("};")

    sb.toString()
  }

  def forLoopGenCode(forIR: ForIR, method: MethodIR): String = {

    val variableName = forIR.identifierIR.name
    val sb = new StringBuilder()
    sb.append(genCode(forIR.expressionIR, method))
    sb.append(s".forEach($variableName => {")
    sb.append(genCode(forIR.blockIR, method))
    sb.append("});")

    sb.toString()
  }

  def genCode(expression: ExpressionIR, method: MethodIR): String = {

    expression match {
      case ArrayValueIR(expressionIRs) =>"[" +  expressionIRs.map(expression => genCode(expression, method)).mkString(",") + "]"
      case identifier: IdentifierIR => identifierGenCode(identifier)
      case doubleConst: DoubleConstIR => doubleConst.value.toString
      case floatConst: FloatConstIR => floatConst.value.toString
      case intConst: IntConstIR => intConst.value.toString
      case longConst: LongConstIR => longConst.value.toString
      case stringLiteral: StringLiteralIR => "\"" + stringLiteral.value + "\""
      case rBinaryIR: RBinaryIR => genCode(rBinaryIR.expressionIR1, method) + genCode(rBinaryIR.operatorIR, method) + genCode(rBinaryIR.expressionIR2, method)
      case PrintlnIR(name, expressions) => "console.log(" + expressions.map(expression => genCode(expression, method)).mkString(",")+  ")"
      case MethodCallIR(name, expressions) => name + "(" + expressions.map(expression => genCode(expression, method)).mkString(",")+  ")"
    }
  }

  def identifierGenCode(identifierIR: IdentifierIR): String = {
    identifierIR.name
  }

  def genCode(statement: StatementIR, method: MethodIR): String = {

    statement match {
      case identifierIR: IdentifierIR => identifierIR.name
      case doBlock: DoBlockIR => genCode(doBlock.asInstanceOf[BlockIR], method)
      case forIR: ForIR => forLoopGenCode(forIR, method)
      case exprAsStmt: ExprAsStmtIR => genCode(exprAsStmt.expressionIR, method) + ";"
      case DAdd => "+"
      case DSub => "-"
      case DMul => "*"
      case DDiv => "/"
      case FAdd => "+"
      case FSub => "-"
      case FMul => "*"
      case FDiv => "/"
      case IAdd => "+"
      case ISub => "-"
      case IMul => "*"
      case IDiv => "/"
      case LAdd => "+"
      case LSub => "-"
      case LMul => "*"
      case LDiv => "/"
      case LessIR => "<"
      case LessEqualIR => "<="
      case GreaterIR => ">"
      case GreaterEqualIR => ">="
      case EqualIR => "=="
      case inline: InlineIR => genCode(inline.asInstanceOf[BlockIR], method)
      case AssignIR(name, immutable, block) => {
        val sb = new StringBuilder()
        sb.append(s"var $name = ")
        sb.append(genCode(block, method))
        sb.append(";")
        sb.toString()
      }
      case IfStatementIR(condition, isStmt, elseStmt) => {
        val sb = new StringBuilder()
        sb.append("if(")
        sb.append(genCode(condition, method))
        sb.append("){")
        sb.append(genCode(isStmt, method))
        sb.append("}")
        if(elseStmt.isDefined) {
          sb.append("else{")
          sb.append(genCode(elseStmt.get, method))
          sb.append("}")
        }
        sb.toString()
      }
    }
  }

  def genCode(block: BlockIR, method: MethodIR): String = {
    block match {
      case doBlock: DoBlockIR => doBlock.statements.map(statement => genCode(statement, method)).mkString
      case inline: InlineIR => genCode(inline.expression, method)
    }
  }
}
