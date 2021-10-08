package transpiler.parser.ast

trait Operator

trait ABinOp extends Operator

case object Add extends ABinOp

case object Subtract extends ABinOp

case object Multiply extends ABinOp

case object Divide extends ABinOp

trait BBinOp extends Operator

case object And extends BBinOp

case object Or extends BBinOp

trait RBinOp extends Operator

case object GreaterEqual extends RBinOp

case object Greater extends RBinOp

case object LessEqual extends RBinOp

case object Less extends RBinOp

case object Equal extends RBinOp

