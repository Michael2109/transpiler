package transpiler.js

trait ExpressionJS

case class ArrayValueJS(expressionIRs: Seq[ExpressionJS]) extends ExpressionJS

case class BoolConstJS(value: Boolean) extends ExpressionJS

case class PrintLnJS(name: String, expressions: Seq[ExpressionJS]) extends ExpressionJS

case class MethodCallJS(name: String, expressions: Seq[ExpressionJS]) extends ExpressionJS

case class DoubleConstJS(value: BigDecimal) extends ExpressionJS

case class FloatConstJS(value: BigDecimal) extends ExpressionJS

case class IdentifierJS(name: String) extends ExpressionJS

case class IntConstJS(value: BigInt) extends ExpressionJS

case class LongConstJS(value: BigInt) extends ExpressionJS

case class StringLiteralJS(value: String) extends ExpressionJS

case class RBinaryJS(operatorJS: RelationalOperatorJS, expressionIR1: ExpressionJS, expressionIR2: ExpressionJS) extends ExpressionJS

trait RelationalOperatorJS extends StatementJS

case object GreaterEqualJS$ extends RelationalOperatorJS

case object GreaterJS$ extends RelationalOperatorJS

case object LessEqualJS$ extends RelationalOperatorJS

case object LessJS$ extends RelationalOperatorJS

case object EqualJS$ extends RelationalOperatorJS

trait ArithmeticOperatorJS extends StatementJS

case object AddJS extends ArithmeticOperatorJS

case object SubJS extends ArithmeticOperatorJS

case object MulJS extends ArithmeticOperatorJS

case object DivJS extends ArithmeticOperatorJS

