package transpiler.parser.ast

trait TypeRel

case class Inherits() extends TypeRel

case class Extends() extends TypeRel

case class Equals() extends TypeRel
