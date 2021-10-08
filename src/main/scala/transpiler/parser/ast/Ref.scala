package transpiler.parser.ast

trait Ref

case class RefSpecial(specialRef: SpecialRef) extends Ref

case class RefLocal(name: Name) extends Ref

case class RefQual(qualName: QualName) extends Ref

trait SpecialRef

case class Super() extends SpecialRef

case class This() extends SpecialRef
