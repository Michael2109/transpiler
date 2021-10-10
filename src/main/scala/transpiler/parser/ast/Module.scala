package transpiler.parser.ast


case class Module(header: ModuleHeader, models: Seq[Model])

case class ModuleHeader(nameSpace: Package, imports: Seq[Import])

case class Import(loc: Seq[Name])

case class Type(ref: Ref)

case class Package(nameSpace: Seq[Name])

case class Annotation(name: Name)


