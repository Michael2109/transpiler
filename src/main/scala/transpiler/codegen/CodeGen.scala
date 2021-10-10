package transpiler.codegen

import transpiler.js.{ArrayValueIR, AssignIR, BlockIR, DAdd, DDiv, DMul, DSub, DoBlockIR, DoubleConstIR, EqualIR, ExprAsStmtIR, ExpressionIR, FAdd, FDiv, FMul, FSub, FieldIR, FloatConstIR, ForIR, GreaterEqualIR, GreaterIR, IAdd, IDiv, IMul, ISub, IdentifierIR, IfStatementIR, InlineIR, IntConstIR, LAdd, LDiv, LMul, LSub, LessEqualIR, LessIR, LongConstIR, MethodCallIR, MethodIR, ModelIR, PrintlnIR, RBinaryIR, StatementIR, StringLiteralIR}
import transpiler.parser.ast._;

object CodeGen {

  def genModelCode(model: ModelIR): String = {

    val sb: StringBuilder = new StringBuilder()

    sb.append(s"class ${model.name} {")
    sb.append(model.fields.map(field => fieldGenCode(field)).mkString)

    //  classModel.externalStatements.foreach(v => fieldGenCode(stringBuilder, v))
    sb.append(model.methods.map(m => methodGenCode(m)).mkString)

    sb.append("};")


    sb.toString()
  }

  def fieldGenCode(field: FieldIR): String = {
    val sb = new StringBuilder()
    sb.append(s"${field.name}(){")
    if(field.init.isDefined) {
      sb.append("return ")
      sb.append(statementGenCode(field.init.get))
      sb.append("}")
    }

    sb.toString()
  }

  def methodGenCode(method: MethodIR): String = {
    val sb = new StringBuilder()
    sb.append(method.name)
    sb.append("(")
    sb.append(method.fields.map(_._1).mkString(","))
    sb.append("){")
    sb.append(blockGenCode(method.body))
    sb.append("};")

    sb.toString()
  }

  def forLoopGenCode(forIR: ForIR): String = {

    val variableName = forIR.identifierIR.name
    val sb = new StringBuilder()
    sb.append(expressionGenCode(forIR.expressionIR))
    sb.append(s".forEach($variableName => {")
    sb.append(blockGenCode(forIR.blockIR))
    sb.append("});")

    sb.toString()
  }

  def expressionGenCode(expression: ExpressionIR): String = {

    expression match {
      case ArrayValueIR(expressionIRs) => "[" + expressionIRs.map(expression => expressionGenCode(expression)).mkString(",") + "]"
      case identifier: IdentifierIR => identifierGenCode(identifier)
      case doubleConst: DoubleConstIR => doubleConst.value.toString
      case floatConst: FloatConstIR => floatConst.value.toString
      case intConst: IntConstIR => intConst.value.toString
      case longConst: LongConstIR => longConst.value.toString
      case stringLiteral: StringLiteralIR => "\"" + stringLiteral.value + "\""
      case rBinaryIR: RBinaryIR => expressionGenCode(rBinaryIR.expressionIR1) + statementGenCode(rBinaryIR.operatorIR) + expressionGenCode(rBinaryIR.expressionIR2)
      case PrintlnIR(name, expressions) => "console.log(" + expressions.map(expression => expressionGenCode(expression)).mkString(",") + ")"
      case MethodCallIR(name, expressions) => name + "(" + expressions.map(expression => expressionGenCode(expression)).mkString(",") + ")"
    }
  }

  def identifierGenCode(identifierIR: IdentifierIR): String = {
    identifierIR.name
  }

  def statementGenCode(statement: StatementIR): String = statement match {
    case identifierIR: IdentifierIR => identifierIR.name
    case doBlock: DoBlockIR => blockGenCode(doBlock.asInstanceOf[BlockIR])
    case forIR: ForIR => forLoopGenCode(forIR)
    case exprAsStmt: ExprAsStmtIR => expressionGenCode(exprAsStmt.expressionIR) + ";"
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
    case inline: InlineIR => blockGenCode(inline.asInstanceOf[BlockIR])
    case AssignIR(name, immutable, block) => {
      val sb = new StringBuilder()
      sb.append(s"var $name = ")
      sb.append(blockGenCode(block))
      sb.append(";")
      sb.toString()
    }
    case IfStatementIR(condition, isStmt, elseStmt) => {
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

  def blockGenCode(block: BlockIR): String = {
    block match {
      case doBlock: DoBlockIR => doBlock.statements.map(statement => statementGenCode(statement)).mkString
      case inline: InlineIR => expressionGenCode(inline.expression)
    }
  }
}
