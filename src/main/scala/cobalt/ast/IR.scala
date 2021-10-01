package cobalt.ast

import cobalt.ast.AST._
import cobalt.ast.IRNew._
import cobalt.jar_loader.JarUtility
import cobalt.symbol_table.{ClassEntry, MethodEntry, SymbolTable, ValueEntry}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
        classModel.body.foreach(s =>{
          convertToIR(s, classModelIR, classSymbolTable, imports)
        })

        addConstructor(classModelIR, superClass)
        classModelIR
      }
    })
  }

  def addConstructor(model: ClassModelIR, superClass: String): Unit = {
    if (!constructorExists()) {
      val methodIR = MethodIR("<init>", mutable.SortedSet[Int](Opcodes.ACC_PUBLIC), ListBuffer(), "V", ListBuffer())

      methodIR.body += VisitVarInsn(Opcodes.ALOAD, 0)
      methodIR.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, superClass, "<init>", "()V")
      methodIR.body += VisitInsn(Opcodes.RETURN)

      model.methods += methodIR
    }
  }

  def constructorExists(): Boolean = {
    return false
  }

  def convertToIR(statement: Statement, model: ClassModelIR, symbolTable: SymbolTable, imports: Map[String, String]): Unit = {
    statement match {
      case assign: Assign => {
        val name = assign.name.value
        val id = model.getNextVarId()
        val typeIR = IRUtils.typeStringToTypeIR(assign.`type`.get.ref match {
          case refLocal: RefLocal => refLocal.name.value
          case refQual: RefQual => refQual.qualName.name.value
        })
        val bytecodeType: String = IRUtils.typeToBytecodeType(typeIR)

        model.externalStatements += VisitField(id, name, bytecodeType, null, null)
        val methodIR = MethodIR(assign.name.value, mutable.SortedSet[Int](), ListBuffer(), bytecodeType, ListBuffer())
        symbolTable.entries += new ValueEntry(name, id, typeIR)

        convertToIR(assign.block, methodIR, symbolTable, imports)

        methodIR.body += VisitInsn(Opcodes.RETURN)

        model.methods += methodIR
      }
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, model, symbolTable, imports))
      case method: Method => {

        val typeIR = IRUtils.typeStringToTypeIR(method.returnType.get.ref match {
          case refLocal: RefLocal => refLocal.name.value
          case refQual: RefQual => refQual.qualName.name.value
        })
        val bytecodeType: String = IRUtils.typeToBytecodeType(typeIR)

        val methodIR = MethodIR(method.name.value, mutable.SortedSet[Int](), ListBuffer(), bytecodeType, ListBuffer())

        methodIR.modifiers += Opcodes.ACC_PUBLIC

        methodIR.modifiers ++ method.modifiers.map(IRUtils.modifierToOpcode)

        if (methodIR.name.equals("main")) {
          methodIR.modifiers += Opcodes.ACC_STATIC
          methodIR.fields += (("args", "[Ljava/lang/String;"))
        } else {
          method.fields.foreach(f => {
            val t = f.`type`.ref match {
              case RefLocal(name) => name.value
              case RefQual(qualName) => qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
            }
            methodIR.fields += ((f.name.value, IRUtils.typeToBytecodeType(IRUtils.typeStringToTypeIR(t))))
          })
        }
        val methodSymbolTable: SymbolTable = new MethodEntry(methodIR.name, "")

        convertToIR(method.body, methodIR, methodSymbolTable, imports)
        methodIR.body += VisitInsn(Opcodes.RETURN)

        model.methods += methodIR
      }
    }
  }

  def convertToIR(operator: Operator, `type`: TypeIR, method: MethodIR, symbolTable: SymbolTable): Unit = {
    `type` match {
      case _: DoubleType => operator match {
        case Add => method.body += DAdd
        case Subtract => method.body += DSub
        case Multiply => method.body += DMul
        case Divide => method.body += DDiv
      }
      case _: FloatType => operator match {
        case Add => method.body += FAdd
        case Subtract => method.body += FSub
        case Multiply => method.body += FMul
        case Divide => method.body += FDiv
      }
      case _: IntType => operator match {
        case Add => method.body += IAdd
        case Subtract => method.body += ISub
        case Multiply => method.body += IMul
        case Divide => method.body += IDiv
      }
      case _: LongType => operator match {
        case Add => method.body += LAdd
        case Subtract => method.body += LSub
        case Multiply => method.body += LMul
        case Divide => method.body += LDiv
      }
      case _: ObjectType => operator match {
        case Add =>
        case Subtract =>
        case Multiply =>
        case Divide =>
      }
    }
  }

  def convertToIR(expression: Expression, method: MethodIR, symbolTable: SymbolTable, imports: Map[String, String]): Unit = {
    expression match {
      case aBinary: ABinary => {
        convertToIR(aBinary.expression1, method, symbolTable, imports)
        convertToIR(aBinary.expression2, method, symbolTable, imports)
        convertToIR(aBinary.op, IRUtils.inferType(aBinary.expression1, symbolTable, imports), method, symbolTable)
      }
      case blockExpr: BlockExpr => blockExpr.expressions.foreach(e => convertToIR(e, method, symbolTable, imports))
      case doubleConst: DoubleConst => method.body += ExprAsStmtIR(DoubleConstIR(doubleConst.value))
      case floatConst: FloatConst => method.body += ExprAsStmtIR(FloatConstIR(floatConst.value))
      case identifier: Identifier => {
        identifier.name.value match {
          case "true" => convertToIR(IntConst(1), method, symbolTable, imports)
          case "false" => convertToIR(IntConst(0), method, symbolTable, imports)
          case _ => {

            val (id, typeIR) = symbolTable.get(identifier.name.value) match {
              case v: ValueEntry => (v.id, v.`type`)
            }


            boxExpressionStart(typeIR, method)

            method.body += ExprAsStmtIR(IdentifierIR(id, typeIR))

            boxExpressionEnd(typeIR, method)
          }
        }
      }
      case intCont: IntConst => method.body += ExprAsStmtIR(IntConstIR(intCont.value))
      case longConst: LongConst => method.body += ExprAsStmtIR(LongConstIR(longConst.value))
      case methodCall: MethodCall => {
        methodCall.name.value match {
          case "print" | "println" => {
            val typeIR = IRUtils.inferType(methodCall.expression.head, symbolTable, imports)
            method.body += VisitFieldInst(Opcodes.GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")

            boxExpressionStart(typeIR, method)
            methodCall.expression.foreach(e => convertToIR(e, method, symbolTable, imports))
            boxExpressionEnd(typeIR, method)

            method.body += VisitMethodInsn(Opcodes.INVOKEVIRTUAL, "java/io/PrintStream", "println", "(Ljava/lang/Object;)V")
          }
          case _ => {
            // Get the type of the method call
            val typeIR = IRUtils.inferType(methodCall.expression.head, symbolTable, imports)
          }
        }
      }
      case nestedExpression: NestedExpr => {

        var currentType: TypeIR = null

        // Loop through all method calls and variables
        nestedExpression.expressions.foreach(e => {
          e match {
            case methodCall: MethodCall => {

              // Get the method argument types and convert to bytecode types
              val argumentTypes = methodCall.expression.map(e => IRUtils.typeToBytecodeType(IRUtils.inferType(e, symbolTable, imports))).filter(!_.isEmpty).toList

              println(JarUtility.getBytecodeClass(currentType.classLoc))
              println(currentType.classLoc + " : " + argumentTypes)
              val signature = JarUtility.getBytecodeClass(currentType.classLoc).getMethod(methodCall.name.value, argumentTypes).getSignature()

              method.body += VisitMethodInsn(Opcodes.INVOKEVIRTUAL, currentType.classLoc, methodCall.name.value, signature)
            }
            case value: Identifier => {

              currentType = symbolTable.get(value match {
                case methodCall: MethodCall => methodCall.name.value
                case identifier: Identifier => identifier.name.value
              }) match {
                case valueEntry: ValueEntry => valueEntry.`type`
              }

              convertToIR(value, method, symbolTable, imports)
            }
          }
        })
      }
      case newClassInstance: NewClassInstance => {
        val className = newClassInstance.`type`.ref match {
          case RefLocal(name) => imports.get(name.value).get
          case RefQual(qualName) => qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
        }

        method.body += VisitTypeInst(Opcodes.NEW, className)
        method.body += VisitInsn(Opcodes.DUP)

        newClassInstance.expression.foreach(e => {
          convertToIR(e, method, symbolTable, imports)
        })

        method.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, className, "<init>", "()V")

      }
      case stringLiteral: StringLiteral => method.body += ExprAsStmtIR(StringLiteralIR(stringLiteral.value))
    }
  }

  def convertToIR(statement: Statement, method: MethodIR, symbolTable: SymbolTable, imports: Map[String, String]): Unit = {
    statement match {
      case assign: Assign => {
        val id = method.getNextVarId()

        val typeIR = IRUtils.typeStringToTypeIR(assign.`type`.get.ref match {
          case refLocal: RefLocal => refLocal.name.value
          case refQual: RefQual => refQual.qualName.name.value
        })

        symbolTable.entries += new ValueEntry(assign.name.value, id, typeIR)
        convertToIR(assign.block, method, symbolTable, imports)
        method.body += IRUtils.getStoreOperator(typeIR, id)
      }
      case blockStmt: BlockStmt => blockStmt.statements.foreach(s => convertToIR(s, method, symbolTable, imports))
      case inline: Inline => convertToIR(inline.expression, method, symbolTable, imports)
      case doBlock: DoBlock => {
        doBlock.statement.foreach(s =>{
          convertToIR(s, method, symbolTable, imports)
        })
      }
      case exprAsStmt: ExprAsStmt => convertToIR(exprAsStmt.expression, method, symbolTable, imports)
      case ifStmt: If => {

        convertToIR(ifStmt.condition, method, symbolTable, imports)
        val trueLabel = method.createLabel()
        val endLabel = method.createLabel()

        method.body += VisitJumpInst(Opcodes.IFEQ, trueLabel)

        convertToIR(ifStmt.ifBlock, method, symbolTable, imports)

        method.body += VisitJumpInst(Opcodes.GOTO, endLabel)

        method.visitLabel(trueLabel)

        if (ifStmt.elseBlock.isDefined) {
          convertToIR(ifStmt.elseBlock.get, method, symbolTable, imports)
        }

        method.visitLabel(endLabel)
      }
    }
  }

  def boxExpressionStart(typeIR: TypeIR, method: MethodIR): Unit ={
    typeIR match {
      case _: DoubleType => {
        method.body += VisitTypeInst(Opcodes.NEW, "java/lang/Double")
        method.body += VisitInsn(Opcodes.DUP)
      }
      case _: FloatType => {
        method.body += VisitTypeInst(Opcodes.NEW, "java/lang/Float")
        method.body += VisitInsn(Opcodes.DUP)
      }
      case _: IntType => {
        method.body += VisitTypeInst(Opcodes.NEW, "java/lang/Integer")
        method.body += VisitInsn(Opcodes.DUP)
      }
      case _: LongType => {
        method.body += VisitTypeInst(Opcodes.NEW, "java/lang/Long")
        method.body += VisitInsn(Opcodes.DUP)
      }
      case _ =>
    }
  }

  def boxExpressionEnd(typeIR: TypeIR, method: MethodIR): Unit ={
    typeIR match {
      case _: DoubleType => {
        method.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Double", "<init>", "(D)V")
      }
      case _: FloatType => {
        method.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Float", "<init>", "(F)V")
      }
      case _: IntType => {
        method.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Integer", "<init>", "(I)V")
      }
      case _: LongType => {
        method.body += VisitMethodInsn(Opcodes.INVOKESPECIAL, "java/lang/Long", "<init>", "(J)V")
      }
      case _ =>
    }
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

  case class MethodIR(name: String, modifiers: mutable.SortedSet[Int], fields: ListBuffer[(String, String)], returnType: String, body: ListBuffer[StatementIR]) {

    private var nextVarId = 0

    def getNextVarId(): Int = {
      val id = nextVarId
      nextVarId += 1
      return id
    }

    val labels: mutable.SortedMap[Int, Label] = mutable.SortedMap[Int, Label]()

    def createLabel(): Int = {
      labels.put(labels.size, new Label)
      val id = labels.size - 1
      id
    }

    def visitLabel(id: Int): Unit = {
      body += VisitLabelIR(id)
    }
  }


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

}
