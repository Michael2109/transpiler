package transpiler.js

import transpiler.parser.ast.{Abstract, ArrayValue, Assign, Block, CurlyBracketsBlock, Equal, ExprAsStmt, Expression, Field, Final, For, Greater, GreaterEqual, Identifier, If, Inline, IntConst, Less, LessEqual, Method, MethodCall, Model, Module, PackageLocal, Private, Protected, Pure, RBinOp, RBinary, RefLocal, RefQual, Statement, StringLiteral}

import scala.collection.mutable.ListBuffer

object AST2IR {

  def moduleToIR(module: Module): Seq[ModelIR] = {
    module.models.map(model => modelToIR(model, module))
  }

  def modelToIR(model: Model, module: Module): ModelIR = {

    // TODO Remove imports if not needed?
    val imports: Map[String, String] = module.header.imports.map(i => i.loc.last.value -> i.loc.map(_.value).mkString("/")).toMap[String, String]

    val superClass: Option[String] = model.parent match {
      case Some(value) => value.ref match {
        case RefLocal(name) => Option(imports.get(name.value).get)
        case RefQual(qualName) => Option(qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + qualName.name.value)
      }
      case None => None
    }

    val traits: Seq[String] = model.interfaces.map(i => {
      i.ref match {
        case refLocal: RefLocal => imports(refLocal.name.value)
        case refQual: RefQual => refQual.qualName.nameSpace.nameSpace.map(_.value).mkString("/") + "/" + refQual.qualName.name.value
      }
    })

    val (fields, methods) = model.body.partition(_.isInstanceOf[Field])

    val fieldsIR = fields.map(_.asInstanceOf[Field]).map(fieldToIr)
    val methodsIR = methods.map(_.asInstanceOf[Method]).map(methodToIR)

    val classModelIR = ModelIR(model.name.value, superClass, traits, fieldsIR, methodsIR)

    classModelIR
  }

  def fieldToIr(field: Field): FieldIR = {
    FieldIR(field.name.value, field.`type`.ref.toString, field.init.map(statementToIR).headOption)
  }

  def methodToIR(method: Method): MethodIR = {

    // name: String, modifiers: mutable.SortedSet[String], fields: ListBuffer[(String, String)], returnType: String, body: ListBuffer[StatementIR]
    val modifiers: List[ModifierIR] = method.modifiers.map {
      case Private => PrivateIR()
      case Protected => ProtectedIR()
      case Final => FinalIR()
      case Pure => PureIR()
      case PackageLocal => PackageLocalIR()
      case Abstract => AbstractIR()
    }.toList

    // val fields = method.fields.map(field => field.)

    //  val returnType = method.returnType.map(rT => rT.ref)

    MethodIR(method.name.value, modifiers, ListBuffer(), "void", blockToIR(method.body))
  }


  def blockToIR(block: Block): BlockIR = {
    block match {
      case inline: Inline => inlineToIR(inline)
      case doBlock: CurlyBracketsBlock => doBlockToIR(doBlock)
    }

  }

  def inlineToIR(inline: Inline): InlineIR = {
    val expressionIR = expressionToIR(inline.expression)
    InlineIR(expressionIR)
  }

  def doBlockToIR(doBlock: CurlyBracketsBlock): DoBlockIR = {
    val statements = doBlock.statements.map(statement => {
      statementToIR(statement)
    }).toList
    DoBlockIR(statements)
  }

  def expressionToIR(expression: Expression): ExpressionIR = expression match {
    case arrayValue: ArrayValue => ArrayValueIR(arrayValue.expressions.map(a => expressionToIR(a)))
    case intConst: IntConst => IntConstIR(intConst.value)
    case stringLiteral: StringLiteral => StringLiteralIR(stringLiteral.value)
    case identifier: Identifier => identifierToIR(identifier)
    case methodCall: MethodCall => methodCallToIR(methodCall)
    case rBinary: RBinary => RBinaryIR(relationalOpToIR(rBinary.op), expressionToIR(rBinary.expression1), expressionToIR(rBinary.expression2))
  }

  def relationalOpToIR(op: RBinOp): RelationalOperatorIR = op match {
    case GreaterEqual => GreaterEqualIR
    case Greater => GreaterIR
    case LessEqual => LessEqualIR
    case Less => LessIR
    case Equal => EqualIR
  }

  def methodCallToIR(methodCall: MethodCall): ExpressionIR = {

    if (methodCall.name.value == "println") {
      PrintlnIR(methodCall.name.value, methodCall.expression.map(expression => expressionToIR(expression)))
    } else {
      MethodCallIR(methodCall.name.value, methodCall.expression.map(expression => expressionToIR(expression)))
    }
  }

  def identifierToIR(identifier: Identifier): IdentifierIR = {
    IdentifierIR(identifier.name.value)
  }


  def ifToIR(ifStatement: If): IfStatementIR = {
    //  condition: ExpressionIR, isStmt: StatementIR, elseStmt: StatementIR
    val elseBlock = if (ifStatement.elseBlock.isDefined) statementToIR(ifStatement.elseBlock.get) else null

    IfStatementIR(expressionToIR(ifStatement.condition), statementToIR(ifStatement.ifBlock), Option(elseBlock))
    // IdentifierIR(identifier.name.value, null)
  }

  def statementToIR(statement: Statement): StatementIR = {
    statement match {
      case Assign(name, _, immutable, block) => AssignIR(name.value, immutable, blockToIR(block))
      case For(identifier, expression, block) => ForIR(identifierToIR(identifier), expressionToIR(expression), blockToIR(block))
      case ifStatement: If => ifToIR(ifStatement)
      case doBlock: CurlyBracketsBlock => doBlockToIR(doBlock)
      case exprAsStmt: ExprAsStmt => ExprAsStmtIR(expressionToIR(exprAsStmt.expression))
    }
  }
}
