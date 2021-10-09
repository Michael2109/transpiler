package transpiler.parser.ast

trait Expression

case class BlockExpr(expressions: Seq[Expression]) extends Expression

case class NestedExpr(expressions: Seq[Expression]) extends Expression

case class ArrayValue(expressions: Seq[Expression]) extends Expression

case class Identifier(name: Name) extends Expression

case class MethodCall(name: Name, expression: Seq[Expression]) extends Expression

case class NewClassInstance(`type`: Type, expression: Seq[Expression], anonymousClass: Option[Statement]) extends Expression

case class StringLiteral(value: String) extends Expression

case class Ternary(condition: Expression, ifExpr: Expression, elseExpr: Expression) extends Expression

case class Tuple() extends Expression

case class BoolConst(value: Boolean) extends Expression

case class Not() extends Expression

case class ABinary(op: ABinOp, expression1: Expression, expression2: Expression) extends Expression

case class BBinary(op: BBinOp, expression1: Expression, expression2: Expression) extends Expression

case class RBinary(op: RBinOp, expression1: Expression, expression2: Expression) extends Expression

case class IntConst(value: BigInt) extends Expression

case class IntObject(value: Statement) extends Expression

case class LongConst(value: BigInt) extends Expression

case class DoubleConst(value: BigDecimal) extends Expression

case class FloatConst(value: BigDecimal) extends Expression

case class Neg(expression: Expression) extends Expression

case class SpecialRefAsExpr() extends Expression
