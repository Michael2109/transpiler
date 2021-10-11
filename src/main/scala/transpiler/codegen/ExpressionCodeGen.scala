package transpiler.codegen

import transpiler.codegen.StatementCodeGen.{identifierGenCode, statementGenCode}
import transpiler.js.{ArrayValueJS, DoubleConstJS, ExpressionJS, FloatConstJS, IdentifierJS, IntConstJS, LongConstJS, MethodCallJS, PrintLnJS, RBinaryJS, StringLiteralJS}

class ExpressionCodeGen {

  def expressionGenCode(expression: ExpressionJS): String = expression match {
    case ArrayValueJS(expressionIRs) => "[" + expressionIRs.map(expression => expressionGenCode(expression)).mkString(",") + "]"
    case identifier: IdentifierJS => identifierGenCode(identifier)
    case doubleConst: DoubleConstJS => doubleConst.value.toString
    case floatConst: FloatConstJS => floatConst.value.toString
    case intConst: IntConstJS => intConst.value.toString
    case longConst: LongConstJS => longConst.value.toString
    case stringLiteral: StringLiteralJS => "\"" + stringLiteral.value + "\""
    case rBinaryIR: RBinaryJS => expressionGenCode(rBinaryIR.expressionIR1) + statementGenCode(rBinaryIR.operatorJS) + expressionGenCode(rBinaryIR.expressionIR2)
    case PrintLnJS(name, expressions) => "console.log(" + expressions.map(expression => expressionGenCode(expression)).mkString(",") + ")"
    case MethodCallJS(name, expressions) => name + "(" + expressions.map(expression => expressionGenCode(expression)).mkString(",") + ")"
  }

  def identifierGenCode(identifierIR: IdentifierJS): String = {
    identifierIR.name
  }

}
