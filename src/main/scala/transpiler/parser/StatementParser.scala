package transpiler.parser

import fastparse.ScalaWhitespace._
import fastparse._
import transpiler.parser.ast.AST._

object StatementParser {

  def accessModifier[_: P]: P[Modifier] = P("protected").map(_ => Protected()) | P("private").map(_ => Private()) | P("local").map(_ => PackageLocal())

  def annotationParser[_: P]: P[Annotation] = P("@" ~ ExpressionParser.nameParser).map(Annotation)

  def modifiers[_: P]: P[Seq[Modifier]] = P(accessModifier | ExpressionParser.typeModifier).rep

  def NEWLINE[_: P]: P0 = P("\n" | "\r\n" | End)

  def ENDMARKER[_: P]: P0 = P(End)

  def assignParser[_: P]: P[Assign] = P("let" ~ ("mutable").!.? ~ ExpressionParser.nameParser ~ (":" ~ ExpressionParser.typeRefParser).? ~/ P("=") ~ blockParser).map(x => Assign(x._2, x._3, x._1.isEmpty, x._4))

  def blockParser[_: P]: P[Block] = P(curlyBracketsBlock | ExpressionParser.expressionParser.map(Inline))

  def exprAsStmt[_: P]: P[Statement] = P(ExpressionParser.expressionParser).map(ExprAsStmt)

  def ifStatementParser[_: P]: P[If] = {
    val ifParser: P[(Expression, Statement)] = P("if" ~/ ExpressionParser.expressionParser ~ blockParser).map(x => (x._1, x._2))

    val elifP: P[(Expression, Statement)] = P("elif" ~/ ExpressionParser.expressionParser ~ blockParser).map(x => (x._1, x._2))

    val elseP: P[Statement] = P("else" ~/ blockParser).map(x => x)

    def elseParser: P[Statement] = P(elifP ~ elseParser.?).map(x => If(x._1, x._2, x._3)) | P(elseP)


    P(ifParser ~ elseParser.?).map(x => If(x._1, x._2, x._3))
  }

  def forLoopParser[_: P]: P[For] = {
    val forParser = P("for" ~/ ExpressionParser.identifierParser ~ P("in") ~ ExpressionParser.expressionParser ~ blockParser).map(x => (x._1, x._2, x._3))
    P(forParser).map(x => For(x._1, x._2, x._3))
  }

  def importParser[_: P]: P[Import] = P("import" ~/ ExpressionParser.nameParser.rep(sep = ".")).map(Import)

  def fieldParser[_: P]: P[Field] = P(ExpressionParser.nameParser ~ ":" ~ ExpressionParser.typeRefParser).map(x => Field(x._1, x._2, None))

  def methodParser[_: P]: P[Statement] = P(modifiers ~ "let" ~ ExpressionParser.nameParser ~ "(" ~/ fieldParser.rep(sep = ",") ~ ")" ~ (ExpressionParser.typeRefParser).? ~ blockParser).map(x => Method(x._2, Seq(), x._3, x._1, x._4, x._5))

  def modelParser[_: P]: P[Model] = P("class" ~/ ExpressionParser.nameParser ~ ("extends" ~ ExpressionParser.typeRefParser).? ~ ("with" ~ ExpressionParser.typeRefParser).rep() ~~ curlyBracketsBlock).map(x => ClassModel(x._1, Seq(), Seq(), x._2, Seq(), x._3, x._4.statement))

  def moduleParser[_: P]: P[Module] = P(nameSpaceParser ~ importParser.rep ~ modelParser.rep).map(x => Module(ModuleHeader(x._1, x._2), x._3))

  def nameSpaceParser[_: P]: P[NameSpace] = P("package" ~/ ExpressionParser.nameParser.rep(sep = ".")).map(NameSpace)

  def reassignParser[_: P]: P[Reassign] = P(ExpressionParser.nameParser ~ "<-" ~/ blockParser).map(x => Reassign(x._1, x._2))

  def statementParser[_: P]: P[Statement] = P(modelParser | ifStatementParser | forLoopParser | methodParser | assignParser | reassignParser | exprAsStmt)

  def curlyBracketsBlock[_: P]: P[BraceBlock] = {

    P( "{" ~ LexicalParser.Newline.? ~ componentValue.rep  ~  "}" ).map(x => BraceBlock(x.filter(_.isDefined).map(_.get)))
    /*

    val deeper: P[Int] = {
      val commentLine = P("\n" ~~ LexicalParser.nonewlinewscomment.?.map(_ => 0)).map((_, Some("")))
      val endLine = P("\n" ~~ (" " | "\t").repX(indent + 1).!.map(_.length) ~~ LexicalParser.comment.!.?)
      P(LexicalParser.nonewlinewscomment.? ~~ (endLine | commentLine).repX(1)).map {
        _.collectFirst { case (s, None) => s }
      }.filter(_.isDefined).map(_.get)
    }
    val indented: P[Seq[Statement]] = P(deeper.flatMap { nextIndent =>
      new Statements(nextIndent).statementParser.repX(1, spaces.repX(1) ~~ (" " * nextIndent | "\t" * nextIndent)).map(x => x)
    })
    (indented | (" ".rep ~ statementParser.rep(min = 1, max = 1)))*/
  }


  def componentValue[_: P]: P[Option[Statement]] = {
  //  def blockOpt = P( curlyBracketsBlock  ).map(Some(_))
    P( statementParser.map(Some(_)) |  LexicalParser.Newline.map(_ => None) )
  }
}
