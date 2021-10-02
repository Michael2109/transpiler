package transpiler.parser.ast

import transpiler.parser.ast.AST._
import transpiler.parser.ast.IRNew._
import transpiler.symbol_table.{ClassEntry, SymbolTable}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.tools.asm.{Label, Opcodes}

object AST2IR {

  def astToIR(module: Module): Seq[ModelIR] = {
    module.models.map(model => model match {
      case classModel: ClassModel => {

        val imports: Map[String, String] = module.header.imports.map(i => i.loc.last.value -> i.loc.map(_.value).mkString("/")).toMap[String, String]

        val superClass: String = classModel.parent match {
          case Some(value) => value.ref match {
            case RefLocal(name) => imports.get(name.value).get
            case RefQual(qualName) => qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
          }
          case None => "java/lang/Object"
        }

        val interfaces: Array[String] = classModel.interfaces.map(i => {
          i.ref match {
            case refLocal: RefLocal => imports(refLocal.name.value)
            case refQual: RefQual => refQual.qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + refQual.qualName.name.value
          }
        }).toArray

        val classModelIR = ClassModelIR(module.header.nameSpace.nameSpace.map(_.value).mkString("/"), classModel.name.value, superClass)

        classModelIR.externalStatements += Visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, module.header.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + classModel.name.value, null, superClass, interfaces)

        val classSymbolTable: SymbolTable = new ClassEntry("")
        classModel.body.foreach(s => {
          convertToIR(s, classModelIR, classSymbolTable, imports)
        })

        addConstructor(classModelIR, superClass)
        classModelIR
      }
    })
  }

  def addConstructor(model: ClassModelIR, superClass: String): Unit = {

  }

  def constructorExists(): Boolean = {
    return false
  }

  def convertToIR(statement: Statement, model: ClassModelIR, symbolTable: SymbolTable, imports: Map[String, String]): Unit = {
    statement match {
      case method: Method => model.methods += convertToIR(method, symbolTable, imports)
    }
  }

  def convertToIR(operator: Operator, `type`: TypeIR, method: MethodIR, symbolTable: SymbolTable): Unit = {

  }

  def convertToIR(expression: Expression, method: MethodIR, symbolTable: SymbolTable, imports: Map[String, String]): Unit = {

  }

  def convertToIR(method: Method, symbolTable: SymbolTable, imports: Map[String, String]): MethodIR = {

    // name: String, modifiers: mutable.SortedSet[String], fields: ListBuffer[(String, String)], returnType: String, body: ListBuffer[StatementIR]
    val modifiers: List[ModifierIR] = method.modifiers.map(modifier => modifier match {
      case Public() => PublicIR()
      case Private() => PrivateIR()
      case Protected() => ProtectedIR()
      case Final() => FinalIR()
      case Pure() => PureIR()
      case PackageLocal() => PackageLocalIR()
      case Abstract() => AbstractIR()
    }).toList

   // val fields = method.fields.map(field => field.)

  //  val returnType = method.returnType.map(rT => rT.ref)

    MethodIR(method.name.value, modifiers, ListBuffer(), "void", convertToIR(method.body, symbolTable, imports ))
  }


  def convertToIR(block: Block, symbolTable: SymbolTable, imports: Map[String, String]): List[StatementIR] = {
      List()
  }

  def convertToIR(statement: Statement, method: MethodIR, symbolTable: SymbolTable, imports: Map[String, String]): Unit = {

  }

  def boxExpressionStart(typeIR: TypeIR, method: MethodIR): Unit = {

  }

  def boxExpressionEnd(typeIR: TypeIR, method: MethodIR): Unit = {

  }
}

object IRNew {

  trait ModelIR

  case class ClassModelIR(nameSpace: String, name: String, parent: String) extends ModelIR {

    val traits: ListBuffer[String] = ListBuffer[String]()
    val externalStatements: ListBuffer[StatementIR] = ListBuffer[StatementIR]()
    val methods: ListBuffer[MethodIR] = ListBuffer[MethodIR]()
    val imports: Map[String, String] = Map[String, String]()

    private var nextVarId = 0

    def getNextVarId(): Int = {
      val id = nextVarId
      nextVarId += 1
      return id
    }
  }

  case class MethodIR(name: String, modifiers: List[ModifierIR], fields: ListBuffer[(String, String)], returnType: String, body: List[StatementIR]) {  }


  trait BlockIR extends StatementIR

  case class InlineIR(expression: ExpressionIR) extends BlockIR

  case class DoBlockIR(statement: StatementIR) extends BlockIR


  trait ExpressionIR

  case class AssignIR(id: Int, immutable: Boolean, block: BlockIR) extends ExpressionIR

  case class DoubleConstIR(value: BigDecimal) extends ExpressionIR

  case class FloatConstIR(value: BigDecimal) extends ExpressionIR

  case class IdentifierIR(id: Int, `type`: TypeIR) extends ExpressionIR

  case class IntConstIR(value: BigInt) extends ExpressionIR

  case class LongConstIR(value: BigInt) extends ExpressionIR

  case class StringLiteralIR(value: String) extends ExpressionIR

  trait StatementIR

  case class ExprAsStmtIR(expressionIR: ExpressionIR) extends StatementIR

  case class IfIR(condition: ExpressionIR, isStmt: StatementIR, elseStmt: StatementIR)

  case class LabelIR(id: Int) extends StatementIR

  case class VisitLabelIR(id: Int) extends StatementIR

  trait TypeIR {
    val classLoc: String
  }

  case class IntType() extends TypeIR {
    override val classLoc: String = "java/lang/Integer"
  }

  case class LongType() extends TypeIR {
    override val classLoc: String = "java/lang/Long"
  }

  case class FloatType() extends TypeIR {
    override val classLoc: String = "java/lang/Float"
  }

  case class DoubleType() extends TypeIR {
    override val classLoc: String = "java/lang/Double"
  }

  case class StringLiteralType() extends TypeIR {
    override val classLoc: String = "java/lang/String"
  }

  case class ObjectType(name: String) extends TypeIR {
    override val classLoc: String = name
  }

  case class UnitType() extends TypeIR {
    override val classLoc: String = null
  }

  case class UnknownType() extends TypeIR {
    override val classLoc: String = null
  }

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

  case class PublicIR() extends ModifierIR

  case class ProtectedIR() extends ModifierIR

  case class PrivateIR() extends ModifierIR

  case class PackageLocalIR() extends ModifierIR

  case class AbstractIR() extends ModifierIR

  case class FinalIR() extends ModifierIR

  case class PureIR() extends ModifierIR
}
