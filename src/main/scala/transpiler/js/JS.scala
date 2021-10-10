package transpiler.js

import scala.collection.mutable.ListBuffer


case class ModelIR(name: String, parent: Option[String], traits: Seq[String], fields: Seq[FieldIR], methods: Seq[MethodIR])

case class MethodIR(name: String, modifiers: List[ModifierIR], fields: ListBuffer[(String, String)], returnType: String, body: BlockIR) extends StatementIR

trait BlockIR extends StatementIR

case class InlineIR(expression: ExpressionIR) extends BlockIR

case class DoBlockIR(statements: List[StatementIR]) extends BlockIR

trait RelationalOperatorIR extends StatementIR

case object GreaterEqualIR extends RelationalOperatorIR

case object GreaterIR extends RelationalOperatorIR

case object LessEqualIR extends RelationalOperatorIR

case object LessIR extends RelationalOperatorIR

case object EqualIR extends RelationalOperatorIR

trait ArithmeticOperatorIR extends StatementIR

case object DAdd extends ArithmeticOperatorIR

case object DSub extends ArithmeticOperatorIR

case object DMul extends ArithmeticOperatorIR

case object DDiv extends ArithmeticOperatorIR

case object FAdd extends ArithmeticOperatorIR

case object FSub extends ArithmeticOperatorIR

case object FMul extends ArithmeticOperatorIR

case object FDiv extends ArithmeticOperatorIR

case object IAdd extends ArithmeticOperatorIR

case object ISub extends ArithmeticOperatorIR

case object IMul extends ArithmeticOperatorIR

case object IDiv extends ArithmeticOperatorIR

case object LAdd extends ArithmeticOperatorIR

case object LSub extends ArithmeticOperatorIR

case object LMul extends ArithmeticOperatorIR

case object LDiv extends ArithmeticOperatorIR

trait StoreOperators extends StatementIR

case class AStore(id: Int) extends StoreOperators

case class DStore(id: Int) extends StoreOperators

case class FStore(id: Int) extends StoreOperators

case class IStore(id: Int) extends StoreOperators

case class LStore(id: Int) extends StoreOperators

case class Visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) extends StatementIR

case class VisitField(id: Int, name: String, `type`: String, signature: String, value: Object) extends StatementIR

case class VisitTypeInst(opcode: Int, name: String) extends StatementIR

case class VisitInsn(opcode: Int) extends StatementIR

case class VisitFieldInst(opcode: Int, owner: String, name: String, description: String) extends StatementIR

case class VisitJumpInst(opcode: Int, labelId: Int) extends StatementIR

case class VisitMethodInsn(opcode: Int, owner: String, name: String, description: String) extends StatementIR

case class VisitVarInsn(opcode: Int, id: Int) extends StatementIR


trait ModifierIR

case class ProtectedIR() extends ModifierIR

case class PrivateIR() extends ModifierIR

case class PackageLocalIR() extends ModifierIR

case class AbstractIR() extends ModifierIR

case class FinalIR() extends ModifierIR

case class PureIR() extends ModifierIR
