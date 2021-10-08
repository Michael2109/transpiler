package transpiler.parser.ast

trait Statement

trait Block extends Statement

case class Inline(expression: Expression) extends Block

case class CurlyBracketsBlock(statement: Seq[Statement]) extends Block

case class Model(modelType: ModelType, name: Name, modifiers: Seq[Modifier], fields: Seq[Field], parent: Option[Type], parentArguments: Seq[Expression], interfaces: Seq[Type], body: Seq[Statement]) extends Statement

case class Method(name: Name, annotations: Seq[Annotation], fields: Seq[Field], modifiers: Seq[Modifier], returnType: Option[Type], body: Block) extends Statement

case class For(identifier: Identifier, expression: Expression, block: Block) extends Statement

case class While() extends Statement

case class If(condition: Expression, ifBlock: Statement, elseBlock: Option[Statement]) extends Statement

case class Assign(name: Name, `type`: Option[Type], immutable: Boolean, block: Block) extends Statement

case class AssignMultiple(name: Seq[Name], `type`: Option[Type], immutable: Boolean, block: Block) extends Statement

case class Reassign(name: Name, exprAsStmt: ExprAsStmt) extends Statement

case class Return() extends Statement

case class Lambda() extends Statement

case class ExprAsStmt(expression: Expression) extends Statement

case class BlockStmt(statements: Seq[Statement]) extends Statement

case class Match() extends Statement
