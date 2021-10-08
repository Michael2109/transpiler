package transpiler.parser.ast

case class Name(value: String)

case class QualName(nameSpace: Package, name: Name)
