package transpiler.parser.ast

import transpiler.parser.ast.AST.{Abstract, Block, ClassModel, Expression, Final, Method, Module, Operator, PackageLocal, Private, Protected, Public, Pure, RefLocal, RefQual, Statement}
import transpiler.symbol_table.{ClassEntry, SymbolTable}

import scala.collection.mutable.ListBuffer
import scala.tools.asm.Opcodes


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

    MethodIR(method.name.value, modifiers, ListBuffer(), "void", convertToIR(method.body, symbolTable, imports))
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
