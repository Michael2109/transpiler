package transpiler.codegen

import transpiler.parser.ast._;

object CodeGen {

  def genModelCode(model: ModelIR): String = {

    val sb: StringBuilder = new StringBuilder()

    sb.append(s"class ${model.name} {")
    sb.append(model.fields.map(field => genCode(field)).mkString)

    //  classModel.externalStatements.foreach(v => genCode(stringBuilder, v))
    sb.append(model.methods.map(m => genCode(m)).mkString)

    sb.append("};")


    sb.toString()
  }

  def genCode(field: FieldIR): String = {
    val sb = new StringBuilder()
    sb.append(s"${field.name}(){")
    if(field.init.isDefined) {
      sb.append(genCode(field.init.get))
      sb.append("}")
    }

    sb.toString()
  }

  def genCode(method: MethodIR): String = {
    val sb = new StringBuilder()
    sb.append(method.name)
    sb.append("(")
    sb.append(method.fields.map(_._1).mkString(","))
    sb.append("){")
    sb.append(genCode(method.body))
    sb.append("};")

    sb.toString()
  }

  def forLoopGenCode(forIR: ForIR): String = {

    val variableName = forIR.identifierIR.name
    val sb = new StringBuilder()
    sb.append(genCode(forIR.expressionIR))
    sb.append(s".forEach($variableName => {")
    sb.append(genCode(forIR.blockIR))
    sb.append("});")

    sb.toString()
  }

  def genCode(expression: ExpressionIR): String = {

    expression match {
      case ArrayValueIR(expressionIRs) => "[" + expressionIRs.map(expression => genCode(expression)).mkString(",") + "]"
      case identifier: IdentifierIR => identifierGenCode(identifier)
      case doubleConst: DoubleConstIR => doubleConst.value.toString
      case floatConst: FloatConstIR => floatConst.value.toString
      case intConst: IntConstIR => intConst.value.toString
      case longConst: LongConstIR => longConst.value.toString
      case stringLiteral: StringLiteralIR => "\"" + stringLiteral.value + "\""
      case rBinaryIR: RBinaryIR => genCode(rBinaryIR.expressionIR1) + genCode(rBinaryIR.operatorIR) + genCode(rBinaryIR.expressionIR2)
      case PrintlnIR(name, expressions) => "console.log(" + expressions.map(expression => genCode(expression)).mkString(",") + ")"
      case MethodCallIR(name, expressions) => name + "(" + expressions.map(expression => genCode(expression)).mkString(",") + ")"
    }
  }

  def identifierGenCode(identifierIR: IdentifierIR): String = {
    identifierIR.name
  }

  def genCode(statement: StatementIR): String = {

    statement match {
      case identifierIR: IdentifierIR => identifierIR.name
      case doBlock: DoBlockIR => genCode(doBlock.asInstanceOf[BlockIR])
      case forIR: ForIR => forLoopGenCode(forIR)
      case exprAsStmt: ExprAsStmtIR => genCode(exprAsStmt.expressionIR) + ";"
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
      case inline: InlineIR => genCode(inline.asInstanceOf[BlockIR])
      case AssignIR(name, immutable, block) => {
        val sb = new StringBuilder()
        sb.append(s"var $name = ")
        sb.append(genCode(block))
        sb.append(";")
        sb.toString()
      }
      case IfStatementIR(condition, isStmt, elseStmt) => {
        val sb = new StringBuilder()
        sb.append("if(")
        sb.append(genCode(condition))
        sb.append("){")
        sb.append(genCode(isStmt))
        sb.append("}")
        if (elseStmt.isDefined) {
          sb.append("else{")
          sb.append(genCode(elseStmt.get))
          sb.append("}")
        }
        sb.toString()
      }
    }
  }

  def genCode(block: BlockIR): String = {
    block match {
      case doBlock: DoBlockIR => doBlock.statements.map(statement => genCode(statement)).mkString
      case inline: InlineIR => genCode(inline.expression)
    }
  }
}
