package transpiler.parser.ast

trait Modifier

case object Protected extends Modifier

case object Private extends Modifier

case object PackageLocal extends Modifier

case object Abstract extends Modifier

case object Final extends Modifier

case object Pure extends Modifier
