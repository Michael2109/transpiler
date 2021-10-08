package transpiler.parser.ast

import transpiler.parser.ast.AST.{Abstract, Assign, Block, CurlyBracketsBlock, ExprAsStmt, Expression, Final, For, Identifier, If, Inline, IntConst, Method, Module, PackageLocal, Private, Protected, Public, Pure, RBinary, RefLocal, RefQual, Statement}
import transpiler.symbol_table.{ClassEntry, SymbolTable}

import scala.collection.mutable.ListBuffer

object AST2IR {

  def astToIR(module: Module): Seq[ModelIR] = {
    module.models.map(model => {


      val imports: Map[String, String] = module.header.imports.map(i => i.loc.last.value -> i.loc.map(_.value).mkString("/")).toMap[String, String]

      val superClass: String = model.parent match {
        case Some(value) => value.ref match {
          case RefLocal(name) => imports.get(name.value).get
          case RefQual(qualName) => qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value
        }
        case None => "java/lang/Object"
      }

      val interfaces: Array[String] = model.interfaces.map(i => {
        i.ref match {
          case refLocal: RefLocal => imports(refLocal.name.value)
          case refQual: RefQual => refQual.qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + refQual.qualName.name.value
        }
      }).toArray

      val classModelIR = ClassModelIR(module.header.nameSpace.nameSpace.map(_.value).mkString("/"), model.name.value, superClass)

      // classModelIR.externalStatements += Visit(Opcodes.V1_8, Opcodes.ACC_PUBLIC | Opcodes.ACC_SUPER, module.header.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + classModel.name.value, null, superClass, interfaces)

      val classSymbolTable: SymbolTable = new ClassEntry("")
      model.body.foreach(s => {
        convertToIR(s, classModelIR, classSymbolTable, imports)
      })

      addConstructor(classModelIR, superClass)
      classModelIR
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

    MethodIR(method.name.value, modifiers, ListBuffer(), "void", blockToIR(method.body, symbolTable, imports))
  }


  def blockToIR(block: Block, symbolTable: SymbolTable, imports: Map[String, String]): BlockIR = {
    block match {
      case inline: Inline => inlineToIR(inline, symbolTable, imports)
      case doBlock: CurlyBracketsBlock => doBlockToIR(doBlock, symbolTable, imports)
    }

  }

  def inlineToIR(inline: Inline, symbolTable: SymbolTable, imports: Map[String, String]): InlineIR = {
    val expressionIR = expressionToIR(inline.expression, symbolTable, imports)
    InlineIR(expressionIR)
  }

  def doBlockToIR(doBlock: CurlyBracketsBlock, symbolTable: SymbolTable, imports: Map[String, String]): DoBlockIR = {
    val statements = doBlock.statement.map(statement => {
      statementToIR(statement, symbolTable, imports)
    }).toList
    DoBlockIR(statements)
  }

  def expressionToIR(expression: Expression, symbolTable: SymbolTable, imports: Map[String, String]): ExpressionIR = expression match {
    case intConst: IntConst => IntConstIR(intConst.value)
    case identifier: Identifier => identifierToIR(identifier, symbolTable, imports)
    case rBinary: RBinary => RBinaryIR(expressionToIR(rBinary.expression1, symbolTable, imports), expressionToIR(rBinary.expression2, symbolTable, imports))
  }

  def identifierToIR(identifier: Identifier, symbolTable: SymbolTable, imports: Map[String, String]): IdentifierIR = {
    IdentifierIR(identifier.name.value, null)
  }


  def ifToIR(ifStatement: If, symbolTable: SymbolTable, imports: Map[String, String]): IfStatementIR = {
    //  condition: ExpressionIR, isStmt: StatementIR, elseStmt: StatementIR
    val elseBlock = if (ifStatement.elseBlock.isDefined) statementToIR(ifStatement.elseBlock.get, symbolTable, imports) else null

    IfStatementIR(expressionToIR(ifStatement.condition, symbolTable, imports), statementToIR(ifStatement.ifBlock, symbolTable, imports), elseBlock)
    // IdentifierIR(identifier.name.value, null)
  }

  def statementToIR(statement: Statement, symbolTable: SymbolTable, imports: Map[String, String]): StatementIR = {
    statement match {
      case Assign(name, _, immutable, block) => AssignIR(name.value, immutable, blockToIR(block, symbolTable, imports))
      case For(identifier, expression, block) => ForIR(identifierToIR(identifier, symbolTable, imports), expressionToIR(expression, symbolTable, imports), blockToIR(block, symbolTable, imports))
      case ifStatement: If => ifToIR(ifStatement, symbolTable, imports)
      case doBlock: CurlyBracketsBlock => doBlockToIR(doBlock, symbolTable, imports)
      case exprAsStmt: ExprAsStmt => ExprAsStmtIR(expressionToIR(exprAsStmt.expression, symbolTable, imports))
    }
  }
}
