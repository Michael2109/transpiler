package transpiler.parser

import fastparse.NoWhitespace._
import fastparse._

object LexicalParser {

  //def kw(s: String) = s ~ !(letter | digit | "_")

  def Newline[_: P] = P( NoTrace(StringIn("\r\n", "\n")) )

  def space[_: P] = P( CharsWhileIn(" \r\n", 0) )

  def booleanConst[_: P]: P[Boolean] = P("true" | "false").!.map(b => b == "true")

  def identifier[_: P]: P[String] = P((letter | "_") ~ (letter | digit | "_").rep).!.filter(!keywordList.contains(_))

  def letter[_: P]: P[Unit] = P(lowercase | uppercase)

  def lowercase[_: P]: P[Unit] = P(CharIn("a-z"))

  def uppercase[_: P]: P[Unit] = P(CharIn("A-Z"))

  def digit[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.map(_.toInt))

  val keywordList = Set(
    "and", "del", "from", "not", "while",
    "as", "elif", "global", "or", "with",
    "assert", "else", "if", "pass", "yield",
    "break", "except", "import",
    "class", "exec", "in", "raise",
    "continue", "finally", "is", "return",
    "for", "lambda", "try", "mutable",
    "let"
  )

  def stringLiteral[_: P]: P[String] = P(stringPrefix.? ~ (longString | shortString))

  def stringPrefix[_: P]: P[Unit] = P(
    "r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR" | "b" | "B" | "br" | "Br" | "bR" | "BR"
  )

  def shortString[_: P]: P[String] = P(shortString0("'") | shortString0("\""))

  def shortString0[_: P](delimiter: String): P[String] = P(delimiter ~ shortStringItem(delimiter).rep.! ~ delimiter)

  def shortStringItem[_: P](quote: String): P[Unit] = P(shortStringChar(quote) | escapeSeq)

  def shortStringChar[_: P](quote: String): P[Unit] = P(CharsWhile(!s"\\\n${quote(0)}".contains(_)))

  def longString[_: P]: P[String] = P(longString0("'''") | longString0("\"\"\""))

  def longString0[_: P](delimiter: String): P[String] = P(delimiter ~ longStringItem(delimiter).rep.! ~ delimiter)

  def longStringItem[_: P](quote: String): P[Unit] = P(longStringChar(quote) | escapeSeq | !quote ~ quote.take(1))

  def longStringChar[_: P](quote: String): P[Unit] = P(CharsWhile(!s"\\${quote(0)}".contains(_)))

  def escapeSeq[_: P]: P[Unit] = P("\\" ~ AnyChar)

  def longInteger[_: P]: P[BigInt] = P(integer ~ ("l" | "L"))

  def integer[_: P]: P[BigInt] = (P(octInteger | hexInteger | binInteger | decimalInteger))

  def decimalInteger[_: P]: P[BigInt] = P(nonZeroDigit ~ digit.rep | "0").!.map(scala.BigInt(_))

  def octInteger[_: P]: P[BigInt] = P("0" ~ ("o" | "O") ~ octDigit.rep(1).! | "0" ~ octDigit.rep(1).!).map(scala.BigInt(_, 8))

  def hexInteger[_: P]: P[BigInt] = P("0" ~ ("x" | "X") ~ hexDigit.rep(1).!).map(scala.BigInt(_, 16))

  def binInteger[_: P]: P[BigInt] = P("0" ~ ("b" | "B") ~ binDigit.rep(1).!).map(scala.BigInt(_, 2))

  def nonZeroDigit[_: P]: P[Unit] = P(CharIn("1-9"))

  def octDigit[_: P]: P[Unit] = P(CharIn("0-7"))

  def binDigit[_: P]: P[Unit] = P("0" | "1")

  def hexDigit[_: P]: P[AnyVal] = P(digit | CharIn("a-f", "A-F"))


  def floatNumber[_: P]: P[BigDecimal] = P(pointFloat).!.map(BigDecimal(_))

  def pointFloat[_: P]: P[BigDecimal] = P((intPart.? ~ fraction) | (intPart ~ ".")).!.map(BigDecimal(_))

  def exponentFloat[_: P]: P[BigDecimal] = P((intPart | pointFloat) ~ exponent).!.map(BigDecimal(_))

  def intPart[_: P]: P[BigDecimal] = P(digit.rep(1)).!.map(BigDecimal(_))

  def fraction[_: P]: P[Seq[Int]] = P("." ~ digit.rep(1))

  def exponent[_: P]: P[Seq[Int]] = P(("e" | "E") ~ ("+" | "-").? ~ digit.rep(1))


  def imagnumber[_: P]: P[BigDecimal] = P((floatNumber | intPart) ~ ("j" | "J"))
}
