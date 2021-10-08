package transpiler.parser.ast

import jdk.internal.org.objectweb.asm.Opcodes
import transpiler.parser.ast.AST._
import transpiler.symbol_table.{SymbolTable, ValueEntry}

object IRUtils {

  def typeStringToTypeIR(t: String): TypeIR = {
    t match {
      case "Int" => IntType()
      case "Long" => LongType()
      case "String" => StringLiteralType()
      case "Unit" => UnitType()
      case className => ObjectType(className)
    }
  }

  def typeToBytecodeType(typeIR: TypeIR): String = {
    typeIR match {
      case _: IntType => "I"
      case _: LongType => "J"
      case _: StringLiteralType => "Ljava/lang/String;"
      case _: UnitType => "V"
      case objectType: ObjectType => "L" + objectType.name + ";"
      case _: UnknownType => ""
    }
  }

  def inferType(expression: Expression, symbolTable: SymbolTable, imports: Map[String, String]): TypeIR = {
    expression match {
      case aBinary: ABinary => inferType(aBinary.expression1, symbolTable, imports)
      case blockExpr: BlockExpr => {
        val types = blockExpr.expressions.map(e => inferType(e, symbolTable, imports))
        types.length match {
          case 0 => UnknownType()
          case _ => types.head
        }
      }
      case _: DoubleConst => DoubleType()
      case _: FloatConst => FloatType()
      case identifier: Identifier => ObjectType(symbolTable.get(identifier.name.value) match {
        case v: ValueEntry => v.name
      })
      case _: IntObject => ObjectType("Ljava/lang/Object;")
      case _: IntConst => IntType()
      case _: LongConst => LongType()
      case nestedExpression: NestedExpr => {

        var currentType: TypeIR = null

        // Loop through all method calls and variables
        nestedExpression.expressions.foreach {
          case methodCall: MethodCall => {

            // Get the method argument types and convert to bytecode types
            val argumentTypes = methodCall.expression.map(e => IRUtils.typeToBytecodeType(IRUtils.inferType(e, symbolTable, imports))).toList

            //val signature = JarUtility.getBytecodeClass(currentType.classLoc).getMethod(methodCall.name.value, argumentTypes).getSignature()
          }
          case value: Identifier => {

            currentType = symbolTable.get(value match {
              case methodCall: MethodCall => methodCall.name.value
              case identifier: Identifier => identifier.name.value
            }) match {
              case valueEntry: ValueEntry => valueEntry.`type`
            }
          }
        }

        ObjectType(currentType.classLoc)
      }
      case newClassInstance: NewClassInstance => {
        val superClass: String = newClassInstance.`type`.ref match {
          case RefLocal(name) => imports.get(name.value).getOrElse(name.value)
          case RefQual(qualName) => qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
        }
        ObjectType(superClass)
      }
      case _: StringLiteral => StringLiteralType()
    }
  }

  def getArithmeticOperator(op: Operator, expression1: Expression, expression2: Expression): Int = {

    expression1 match {
      case innerABinary: ABinary => {
        getArithmeticOperator(op, innerABinary.expression1, innerABinary.expression2)
      }
      case _: IntConstIR => {
        op match {
          case Add => Opcodes.IADD
          case Subtract => Opcodes.ISUB
          case Multiply => Opcodes.IMUL
          case Divide => Opcodes.IDIV
        }
      }
      case _: LongConstIR => {
        op match {
          case Add => Opcodes.LADD
          case Subtract => Opcodes.LSUB
          case Multiply => Opcodes.LMUL
          case Divide => Opcodes.LDIV
        }
      }
      case _: FloatConstIR => {
        op match {
          case Add => Opcodes.FADD
          case Subtract => Opcodes.FSUB
          case Multiply => Opcodes.FMUL
          case Divide => Opcodes.FDIV
        }
      }
      case _: DoubleConstIR => {
        op match {
          case Add => Opcodes.DADD
          case Subtract => Opcodes.DSUB
          case Multiply => Opcodes.DMUL
          case Divide => Opcodes.DDIV
        }
      }
    }
  }

  def modifierToOpcode(modifier: Modifier): Int = {
    modifier match {
      case Protected => Opcodes.ACC_PROTECTED
      case Private => Opcodes.ACC_PRIVATE
      case Abstract => Opcodes.ACC_ABSTRACT
      case Final => Opcodes.ACC_FINAL
    }
  }
}
