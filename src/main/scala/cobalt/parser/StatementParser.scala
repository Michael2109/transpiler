package cobalt.parser

import fastparse.noApi._
import WsApi._
import cobalt.ast.AST._
import cobalt.parser

object StatementParser extends Statements(0)

class Statements(indent: Int) {

  val space = P(CharIn(" \n"))
  val NEWLINE: P0 = P("\n" | "\r\n" | End)
  val ENDMARKER: P0 = P(End)
  val indents = P(NEWLINE ~~ " ".repX(indent))
  val spaces = P((LexicalParser.nonewlinewscomment.? ~~ "\n").repX(1))
  val space_indents = P( spaces.repX ~~ " ".repX(indent) )

  val assignParser: P[Assign] = P(LexicalParser.kw("let") ~ ("mutable").!.? ~ ExpressionParser.nameParser ~ (":" ~ ExpressionParser.typeRefParser).? ~/ P(LexicalParser.kw("=")) ~ blockParser).map(x => Assign(x._2, x._3, x._1.isEmpty, x._4))

  val blockParser: P[Block] = P(doBlock | ExpressionParser.expressionParser.map(Inline))

  val commentParser: P[_] = P(LexicalParser.comment)

  val doBlock: P[Block] = P(LexicalParser.kw("do") ~~ indentedBlock).map(x => DoBlock(x))

  val exprAsStmt: P[Statement] = P(ExpressionParser.expressionParser).map(ExprAsStmt)

  val ifStatementParser: P[If] = {
    def ifParser: P[(Expression, Statement)] = P(LexicalParser.kw("if") ~/ ExpressionParser.expressionParser ~ P(LexicalParser.kw("then")) ~ blockParser).map(x => (x._1, x._2))
    def elseParser: P[Statement] = P(elifP ~ elseParser.?).map(x => If(x._1, x._2, x._3)) | P(elseP)

    def elifP: P[(Expression, Statement)] = P(LexicalParser.kw("elif") ~/ ExpressionParser.expressionParser ~ LexicalParser.kw("then") ~ blockParser).map(x => (x._1, x._2))
    def elseP: P[Statement] = P(LexicalParser.kw("else") ~/ blockParser).map(x => x)

    P(ifParser ~ elseParser.?).map(x => If(x._1, x._2, x._3))
  }

  val importParser: P[Import] = P(LexicalParser.kw("import") ~/ ExpressionParser.nameParser.rep(sep=".")).map(Import)

  val fieldParser: P[Field] = P(ExpressionParser.nameParser ~ ":" ~ ExpressionParser.typeRefParser).map(x => Field(x._1, x._2, None))

  val methodParser: P[Statement] = P(ExpressionParser.modifiers ~ LexicalParser.kw("let") ~ ExpressionParser.nameParser ~ "(" ~/ fieldParser.rep(sep = ",") ~ ")" ~ (":" ~ ExpressionParser.typeRefParser).? ~ "=" ~ blockParser).map(x => Method(x._2, Seq(), x._3, x._1, x._4, x._5))

  val modelParser: P[Model] = P(LexicalParser.kw("class") ~/ ExpressionParser.nameParser ~ ("extends" ~ ExpressionParser.typeRefParser).? ~ (LexicalParser.kw("with") ~ ExpressionParser.typeRefParser).rep() ~~ indentedBlock).map(x => ClassModel(x._1, Seq(), Seq(), x._2, Seq(), x._3, x._4))

  val moduleParser: P[Module] = P(nameSpaceParser ~ importParser.rep ~ modelParser.rep).map(x => Module(ModuleHeader(x._1, x._2), x._3))

  val nameSpaceParser: P[NameSpace] = P(LexicalParser.kw("package") ~/ ExpressionParser.nameParser.rep(sep=".")).map(NameSpace)

  val reassignParser: P[Reassign] = P(ExpressionParser.nameParser ~ "<-" ~/ blockParser).map(x => Reassign(x._1, x._2))

  val statementParser: P[Statement] = P(!commentParser ~ (modelParser | ifStatementParser | methodParser | assignParser | reassignParser | exprAsStmt))

  val indentedBlock: P[Seq[Statement]] ={
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
    (indented | (" ".rep ~ statementParser.rep(min = 1, max = 1)))
  }
}
