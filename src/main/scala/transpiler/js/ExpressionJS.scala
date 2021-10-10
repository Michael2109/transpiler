package transpiler.js

trait ExpressionIR

case class ArrayValueIR(expressionIRs: Seq[ExpressionIR]) extends ExpressionIR

case class PrintlnIR( name: String, expressions: Seq[ExpressionIR]) extends ExpressionIR

case class MethodCallIR( name: String, expressions: Seq[ExpressionIR]) extends ExpressionIR

case class DoubleConstIR(value: BigDecimal) extends ExpressionIR

case class FloatConstIR(value: BigDecimal) extends ExpressionIR

case class IdentifierIR(name: String) extends ExpressionIR

case class IntConstIR(value: BigInt) extends ExpressionIR

case class LongConstIR(value: BigInt) extends ExpressionIR

case class StringLiteralIR(value: String) extends ExpressionIR

case class RBinaryIR(operatorIR: RelationalOperatorIR, expressionIR1: ExpressionIR, expressionIR2: ExpressionIR) extends ExpressionIR
