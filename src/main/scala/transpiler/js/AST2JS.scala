package transpiler.js

import transpiler.parser.ast.{Abstract, ArrayValue, Assign, Block, BoolConst, CurlyBracketsBlock, Equal, ExprAsStmt, Expression, Field, Final, For, Greater, GreaterEqual, Identifier, If, Inline, IntConst, Less, LessEqual, Method, MethodCall, Model, Module, PackageLocal, Private, Protected, Pure, RBinOp, RBinary, RefLocal, RefQual, Statement, StringLiteral}

import scala.collection.mutable.ListBuffer

object AST2JS {

  def moduleToIR(module: Module): Seq[ModelJS] = {
    module.models.map(model => modelToIR(model, module))
  }

  def modelToIR(model: Model, module: Module): ModelJS = {

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

    val classModelIR = ModelJS(model.name.value, superClass, traits, fieldsIR, methodsIR)

    classModelIR
  }

  def fieldToIr(field: Field): FieldJS = {
    FieldJS(field.name.value, field.`type`.ref.toString, field.init.map(statementToIR).headOption)
  }

  def methodToIR(method: Method): MethodJS = {

    // name: String, modifiers: mutable.SortedSet[String], fields: ListBuffer[(String, String)], returnType: String, body: ListBuffer[StatementIR]
    val modifiers: List[ModifierJS] = method.modifiers.map {
      case Private => PrivateJS()
      case Protected => ProtectedJS()
      case Final => FinalJS()
      case PackageLocal => PackageLocalJS()
      case Abstract => AbstractJS()
    }.toList

    // val fields = method.fields.map(field => field.)

    //  val returnType = method.returnType.map(rT => rT.ref)

    MethodJS(method.name.value, modifiers, ListBuffer(), "void", blockToIR(method.body))
  }


  def blockToIR(block: Block): BlockJS = {
    block match {
      case inline: Inline => inlineToIR(inline)
      case doBlock: CurlyBracketsBlock => doBlockToIR(doBlock)
    }
  }

  def boolConstToJS(boolConst: BoolConst): BoolConstJS ={
    BoolConstJS(boolConst.value)
  }

  def inlineToIR(inline: Inline): InlineJS = {
    val expressionIR = expressionToIR(inline.expression)
    InlineJS(expressionIR)
  }

  def doBlockToIR(doBlock: CurlyBracketsBlock): DoBlockJS = {
    val statements = doBlock.statements.map(statement => {
      statementToIR(statement)
    }).toList
    DoBlockJS(statements)
  }

  def expressionToIR(expression: Expression): ExpressionJS = expression match {
    case arrayValue: ArrayValue => ArrayValueJS(arrayValue.expressions.map(expressionToIR))
    case boolConst: BoolConst => boolConstToJS(boolConst)
    case identifier: Identifier => identifierToIR(identifier)
    case intConst: IntConst => IntConstJS(intConst.value)
    case methodCall: MethodCall => methodCallToIR(methodCall)
    case rBinary: RBinary => RBinaryJS(relationalOpToIR(rBinary.op), expressionToIR(rBinary.expression1), expressionToIR(rBinary.expression2))
    case stringLiteral: StringLiteral => StringLiteralJS(stringLiteral.value)
  }


  def identifierToIR(identifier: Identifier): IdentifierJS = {
    IdentifierJS(identifier.name.value)
  }


  def ifToIR(ifStatement: If): IfStatementJS = {
    val elseBlock = if (ifStatement.elseBlock.isDefined) Option(statementToIR(ifStatement.elseBlock.get)) else None

    IfStatementJS(expressionToIR(ifStatement.condition), statementToIR(ifStatement.ifBlock), elseBlock)
  }


  def methodCallToIR(methodCall: MethodCall): ExpressionJS = {

    if (methodCall.name.value == "println") {
      PrintLnJS(methodCall.name.value, methodCall.expression.map(expression => expressionToIR(expression)))
    } else {
      MethodCallJS(methodCall.name.value, methodCall.expression.map(expression => expressionToIR(expression)))
    }
  }

  def relationalOpToIR(op: RBinOp): RelationalOperatorJS = op match {
    case GreaterEqual => GreaterEqualJS$
    case Greater => GreaterJS$
    case LessEqual => LessEqualJS$
    case Less => LessJS$
    case Equal => EqualJS$
  }

  def statementToIR(statement: Statement): StatementJS = {
    statement match {
      case Assign(name, _, immutable, block) => AssignJS(name.value, immutable, blockToIR(block))
      case For(identifier, expression, block) => ForJS(identifierToIR(identifier), expressionToIR(expression), blockToIR(block))
      case ifStatement: If => ifToIR(ifStatement)
      case doBlock: CurlyBracketsBlock => doBlockToIR(doBlock)
      case exprAsStmt: ExprAsStmt => ExprAsStmtJS(expressionToIR(exprAsStmt.expression))
    }
  }
}
