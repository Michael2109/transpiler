package transpiler.js

import scala.collection.mutable.ListBuffer

trait StatementJS

case class FieldJS(name: String, `type`: String, init: Option[StatementJS])

case class AssignJS(name: String, immutable: Boolean, block: BlockJS) extends StatementJS

case class ExprAsStmtJS(expressionIR: ExpressionJS) extends StatementJS

case class IfStatementJS(condition: ExpressionJS, isStmt: StatementJS, elseStmt: Option[StatementJS]) extends StatementJS

case class ForJS(identifierIR: IdentifierJS, expressionIR: ExpressionJS, blockIR: BlockJS) extends StatementJS

case class LabelJS(id: Int) extends StatementJS

case class VisitLabelJS(id: Int) extends StatementJS

trait ModifierJS

case class ProtectedJS() extends ModifierJS

case class PrivateJS() extends ModifierJS

case class PackageLocalJS() extends ModifierJS

case class AbstractJS() extends ModifierJS

case class FinalJS() extends ModifierJS

case class ModelJS(name: String, parent: Option[String], traits: Seq[String], fields: Seq[FieldJS], methods: Seq[MethodJS])

case class MethodJS(name: String, modifiers: List[ModifierJS], fields: ListBuffer[(String, String)], returnType: String, body: BlockJS) extends StatementJS

trait BlockJS extends StatementJS

case class InlineJS(expression: ExpressionJS) extends BlockJS

case class DoBlockJS(statements: List[StatementJS]) extends BlockJS

