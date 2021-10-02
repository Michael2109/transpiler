package transpiler.codegen

import transpiler.parser.ast._;

object CodeGen {

  def genModelCode(model: ModelIR): String = {

    val sb: StringBuilder = new StringBuilder()

    println(model)


    model match {
      case classModel: ClassModelIR => {

        sb.append(s"class ${classModel.name} {")

        //  classModel.externalStatements.foreach(v => genCode(stringBuilder, v))
        sb.append(classModel.methods.map(m => genCode(m)).mkString)

        sb.append("}")
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
    sb.append("}")

    sb.toString()
  }

  def genCode(expression: ExpressionIR, method: MethodIR): String = {

    expression match {
      case identifier: IdentifierIR => identifier.id.toString
      case doubleConst: DoubleConstIR => doubleConst.value.toString
      case floatConst: FloatConstIR => floatConst.value.toString
      case intConst: IntConstIR => intConst.value.toString
      case longConst: LongConstIR => longConst.value.toString
      case stringLiteral: StringLiteralIR => "\"" + stringLiteral.value + "\""
    }
  }

  def genCode(statement: StatementIR, method: MethodIR): String = {


    statement match {
      case doBlock: DoBlockIR => genCode(doBlock.asInstanceOf[BlockIR], method)
      case exprAsStmt: ExprAsStmtIR => genCode(exprAsStmt.expressionIR, method)
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
      case inline: InlineIR => genCode(inline.asInstanceOf[BlockIR], method)
      case AssignIR(name, immutable, block) => {
        val sb = new StringBuilder()
        sb.append(s"const $name = ")
        sb.append(genCode(block, method))
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
