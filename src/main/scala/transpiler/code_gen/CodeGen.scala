package transpiler.code_gen

import transpiler.parser.ast._;

object CodeGen {

  val version = 49

  def genModelCode(model: ModelIR): String = {

    val stringBuilder: StringBuilder = new StringBuilder()

    println(model)


    model match {
      case classModel: ClassModelIR => {

        stringBuilder.append(s"class ${classModel.name} {")

        //  classModel.externalStatements.foreach(v => genCode(stringBuilder, v))
        classModel.methods.foreach(m => genCode(stringBuilder, m))

        stringBuilder.append("}")
      }
    }

    stringBuilder.toString()
  }

  def genCode(sb: StringBuilder, method: MethodIR): Unit = {
    println("Method")
    sb.append("function ")
    sb.append(method.name)
    sb.append("(")
    sb.append(method.fields.map(_._1).mkString(","))
    sb.append("){")

    genCode(sb, method.body, method)

    sb.append("}")
  }

  def genCode(sb: StringBuilder, expression: ExpressionIR, method: MethodIR): Unit = {
    println("Expression")
    expression match {
      /* case aBinary: ABinaryIR => {
         genCode(mv, aBinary.expression1)
         genCode(mv, aBinary.expression2)
         val instruction = IRUtils.getArithmeticOperator(aBinary.op, aBinary.expression1, aBinary.expression2)
         mv.visitInsn(instruction)
       }
       case boolConst: BoolConstIR => mv.visitIntInsn(Opcodes.BIPUSH,
         if (boolConst.value.equals(true)) {
           1
         } else {
           0
         })
       case blockStmt: BlockExprIR => blockStmt.expressions.foreach(x => genCode(mv, x))*/
      case identifier: IdentifierIR => sb.append(identifier.id)
      case doubleConst: DoubleConstIR => sb.append(doubleConst.value.toString())
      case floatConst: FloatConstIR => sb.append(floatConst.value.toString())
      case intConst: IntConstIR => sb.append(intConst.value)
      case longConst: LongConstIR => sb.append(longConst.value)
      case stringLiteral: StringLiteralIR => sb.append("\"" + stringLiteral.value + "\"")
    }
  }

  def genCode(sb: StringBuilder, statement: StatementIR, method: MethodIR): Unit = {
    println(statement)
    statement match {
      /*case assign: AssignIR => {
        genCode(mv, assign.block)
        mv.visitVarInsn(IRUtils.getStoreOperator(assign.block), assign.id)
      }*/
      case aStore: AStore => // mv.visitVarInsn(Opcodes.ASTORE, aStore.id);
      /*case blockStmt: BlockStmtIR => blockStmt.statements.foreach(x => genCode(mv, x))*/
      case doBlock: DoBlockIR => genCode(sb, doBlock.asInstanceOf[BlockIR], method)
      case exprAsStmt: ExprAsStmtIR => genCode(sb, exprAsStmt.expressionIR, method)
      /*case ifStmt: IfIR => {
        val trueLabel = new Label
        val endLabel = new Label
        genCode(mv, ifStmt.condition)
        mv.visitJumpInsn(Opcodes.IFEQ, trueLabel)
        genCode(mv, ifStmt.ifBlock)
        mv.visitJumpInsn(Opcodes.GOTO, endLabel)
        mv.visitLabel(trueLabel)
        genCode(mv, ifStmt.elseBlock.getOrElse(BlockStmtIR(Seq())))
        mv.visitLabel(endLabel)
      }*/
      case DAdd => sb.append("+")
      case dStore: DStore => //mv.visitVarInsn(Opcodes.DSTORE, dStore.id);
      case DSub => sb.append("-")
      case DMul => sb.append("*")
      case DDiv => sb.append("/")
      case FAdd => sb.append("+")
      case fStore: FStore => //mv.visitVarInsn(Opcodes.FSTORE, fStore.id);
      case FSub => sb.append("-")
      case FMul => sb.append("*")
      case FDiv => sb.append("/")
      case IAdd => sb.append("+")
      case iStore: IStore => //mv.visitVarInsn(Opcodes.ISTORE, iStore.id);
      case ISub => sb.append("-")
      case IMul => sb.append("*")
      case IDiv => sb.append("/")
      case LAdd => sb.append("+")
      case lStore: LStore => //mv.visitVarInsn(Opcodes.LSTORE, lStore.id);
      case LSub => sb.append("-")
      case LMul => sb.append("*")
      case LDiv => sb.append("/")
      case inline: InlineIR => genCode(sb, inline.asInstanceOf[BlockIR], method)
      case visitFieldInst: VisitFieldInst => //sb.visitFieldInsn(visitFieldInst.opcode, visitFieldInst.owner, visitFieldInst.name, visitFieldInst.description)
      case visitJumpInsn: VisitJumpInst => //mv.visitJumpInsn(visitJumpInsn.opcode, method.labels.get(visitJumpInsn.labelId).get)
      case visitMethodInst: VisitMethodInsn => {
        //      println(visitMethodInst)
        // mv.visitMethodInsn(visitMethodInst.opcode, visitMethodInst.owner, visitMethodInst.name, visitMethodInst.description, false)
      }
      case visitTypeInst: VisitTypeInst => //mv.visitTypeInsn(visitTypeInst.opcode, visitTypeInst.name)
      case visitInst: VisitInsn => // mv.visitInsn(visitInst.opcode)
      case _: LabelIR =>
      case visitLabel: VisitLabelIR => // mv.visitLabel(method.labels.get(visitLabel.id).get)
      case visitVarInsn: VisitVarInsn => // mv.visitVarInsn(visitVarInsn.opcode, visitVarInsn.id)
      case AssignIR(name, immutable, block) => {
        sb.append(s"const $name = ")
        genCode(sb, block, method)
      }
    }
  }

  def genCode(sb: StringBuilder, block: BlockIR, method: MethodIR): Unit = {
    block match {
      case doBlock: DoBlockIR => doBlock.statements.foreach(statement => genCode(sb, statement, method))
      case inline: InlineIR => genCode(sb, inline.expression, method)
    }
  }
}
