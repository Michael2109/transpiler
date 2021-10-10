package transpiler.js

trait StatementIR

case class FieldIR(name: String, `type`: String, init: Option[StatementIR])

case class AssignIR(id: String, immutable: Boolean, block: BlockIR) extends StatementIR

case class ExprAsStmtIR(expressionIR: ExpressionIR) extends StatementIR

case class IfStatementIR(condition: ExpressionIR, isStmt: StatementIR, elseStmt: Option[StatementIR]) extends StatementIR

case class ForIR(identifierIR: IdentifierIR, expressionIR: ExpressionIR, blockIR: BlockIR) extends StatementIR

case class LabelIR(id: Int) extends StatementIR

case class VisitLabelIR(id: Int) extends StatementIR
