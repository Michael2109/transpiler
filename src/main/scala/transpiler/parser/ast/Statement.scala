package transpiler.parser.ast

trait Statement

trait Block extends Statement

case class Inline(expression: Expression) extends Block

case class CurlyBracketsBlock(statements: Seq[Statement]) extends Block

case class Model(modelType: ModelType, name: Name, modifiers: Seq[Modifier], parent: Option[Type], parentArguments: Seq[Expression], interfaces: Seq[Type], body: Seq[Statement]) extends Statement

case class Parameter(name: Name, `type`: Type, init: Option[Expression])

case class Field(name: Name, `type`: Type, init: Option[Statement]) extends Statement

case class Method(name: Name, annotations: Seq[Annotation], fields: Seq[Parameter], modifiers: Seq[Modifier], returnType: Option[Type], body: Block) extends Statement

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

trait ModelType

case object ClassModelType extends ModelType

case object TraitModelType extends ModelType

case object ObjectModelType extends ModelType

trait Modifier

case object Protected extends Modifier

case object Private extends Modifier

case object PackageLocal extends Modifier

case object Abstract extends Modifier

case object Final extends Modifier

case object Pure extends Modifier

case class Module(header: ModuleHeader, models: Seq[Model])

case class ModuleHeader(nameSpace: Namespace, imports: Seq[Import])

case class Import(loc: Seq[Name])

case class Type(ref: Ref)

case class Namespace(nameSpace: Seq[Name])

case class Annotation(name: Name)


case class Name(value: String)

case class QualName(nameSpace: Namespace, name: Name)


trait Ref

case class RefSpecial(specialRef: SpecialRef) extends Ref

case class RefLocal(name: Name) extends Ref

case class RefQual(qualName: QualName) extends Ref

trait SpecialRef

case class Super() extends SpecialRef

case class This() extends SpecialRef

trait TypeRel

case class Inherits() extends TypeRel

case class Extends() extends TypeRel

case class Equals() extends TypeRel
