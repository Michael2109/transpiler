package transpiler.parser

import fastparse.ScalaWhitespace._
import fastparse.{P, _}
import transpiler.parser.ast.AST._

object StatementParser {

  def accessModifier[_: P]: P[Modifier] = P("protected").map(_ => Protected) | P("private").map(_ => Private) | P("local").map(_ => PackageLocal) |  P("final").map(_ => Final)

  def typeModifier[_: P]: P[Modifier] = P("mutable").map(_ => Final) | P("abstract").map(_ => Abstract) | P("pure").map(_ => Pure)

  def annotationParser[_: P]: P[Annotation] = P("@" ~ ExpressionParser.nameParser).map(Annotation)

  def modifiers[_: P]: P[Seq[Modifier]] = P(accessModifier | typeModifier).rep

  def newline[_: P]: P0 = P("\n" | "\r\n" | End)

  def endMarker[_: P]: P0 = P(End)

  def assignParser[_: P]: P[Assign] = P("let" ~ ("mutable").!.? ~ ExpressionParser.nameParser ~ (":" ~ ExpressionParser.typeRefParser).? ~/ P("=") ~ exprAsStmt).map(x => Assign(x._2, x._3, x._1.isEmpty, Inline(x._4.expression)))

  def blockParser[_: P]: P[Block] = P(curlyBracketsBlock)

  def exprAsStmt[_: P]: P[ExprAsStmt] = P(ExpressionParser.expressionParser).map(ExprAsStmt)

  def ifStatementParser[_: P]: P[If] = {
    P(ifParser ~ elseParser.?).map(x => If(x._1, x._2, x._3))
  }

  def forLoopParser[_: P]: P[For] = {
    val forParser = P("for" ~/ ExpressionParser.identifierParser ~ P("in") ~ ExpressionParser.expressionParser ~ blockParser).map(x => (x._1, x._2, x._3))
    P(forParser).map(x => For(x._1, x._2, x._3))
  }

  def importParser[_: P]: P[Import] = P("import" ~/ ExpressionParser.nameParser.rep(sep = ".")).map(Import)

  def fieldParser[_: P]: P[Field] = P(ExpressionParser.nameParser ~ ":" ~ ExpressionParser.typeRefParser).map(x => Field(x._1, x._2, None))

  def methodParser[_: P]: P[Statement] = P(modifiers ~ "let" ~ ExpressionParser.nameParser ~ "(" ~/ fieldParser.rep(sep = ",") ~ ")" ~ (ExpressionParser.typeRefParser).? ~ "=" ~ blockParser).map(x => Method(x._2, Seq(), x._3, x._1, x._4, x._5))

  def modelTypeParser[_: P]: P[ModelType] = P(classModelTypeParser | traitModelTypeParser | objectModelTypeParser)

  def classModelTypeParser[_: P]: P[ModelType] = P("class").map(_ => ClassModelType)

  def traitModelTypeParser[_: P]: P[ModelType] = P("trait").map(_ => TraitModelType)

  def objectModelTypeParser[_: P]: P[ModelType] = P("object").map(_ => ObjectModelType)

  def modelParser[_: P]: P[Model] = P(modelTypeParser ~/ ExpressionParser.nameParser ~ ("extends" ~ ExpressionParser.typeRefParser).? ~ ("with" ~ ExpressionParser.typeRefParser).rep() ~ curlyBracketsBlock).map(x => Model(x._1, x._2, Seq(), Seq(), x._3, Seq(), x._4, x._5.statement))

  def fileParser[_: P]: P[Module] = P(packageParser ~ importParser.rep ~ modelParser.rep).map(x => Module(ModuleHeader(x._1, x._2), x._3))

  def packageParser[_: P]: P[Package] = P("package" ~/ ExpressionParser.nameParser.rep(sep = ".")).map(Package)

  def reassignParser[_: P]: P[Reassign] = P(ExpressionParser.nameParser ~ "=" ~/ exprAsStmt).map(x => Reassign(x._1, x._2))

  def statementParser[_: P]: P[Statement] = P(modelParser | ifStatementParser | forLoopParser | methodParser | assignParser | reassignParser | exprAsStmt)

  def curlyBracketsBlock[_: P]: P[CurlyBracketsBlock] = {
    P("{" ~/ LexicalParser.Newline.? ~ componentValue.rep ~ "}" ~ LexicalParser.space).map(x => CurlyBracketsBlock(x.filter(_.isDefined).map(_.get)))
  }

  def componentValue[_: P]: P[Option[Statement]] = {
    //  def blockOpt = P( curlyBracketsBlock  ).map(Some(_))
    P(statementParser.map(Some(_)) | LexicalParser.Newline.map(_ => None))
  }

  private def ifParser[_: P]: P[(Expression, Statement)] = P("if" ~/ ExpressionParser.expressionParser ~ blockParser).map(x => (x._1, x._2))

  private def elifP[_: P]: P[(Expression, Statement)] = P("elif" ~/ ExpressionParser.expressionParser ~ blockParser).map(x => (x._1, x._2))

  private def elseP[_: P]: P[Statement] = P("else" ~/ blockParser)

  private def elseParser[_: P]: P[Statement] = P(elifP ~ elseParser.?).map(x => If(x._1, x._2, x._3)) | P(elseP)
}
