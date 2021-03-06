package transpiler.parser

import fastparse.ScalaWhitespace._
import fastparse._
import transpiler.parser.ast._


class ExpressionParser {

  /*

    def Chain[_:P](p: P[Expression], op: P[AST.Operator]) = P(p ~ (op ~ p).rep).map {
      case (lhs, chunks) =>
        chunks.foldLeft(lhs) { case (lhs, (operator, rhs)) =>
          operator match {
            case op: ABinOp => new ABinary(op, lhs, rhs)
            case op: BBinOp => new BBinary(op, lhs, rhs)
            case op: RBinOp => new RBinary(op, lhs, rhs)
          }

        }
    }
  */


  def number[_: P]: P[Expression] = P(CharIn("0-9").rep(1)).!.map(i => IntConst(i.toInt))

  def parens[_: P]: P[Expression] = P("(" ~/ addSub ~ ")")

  def factor[_: P]: P[Expression] = methodCallParser | newClassInstanceParser | numberParser | booleanParser | identifierParser | arrayParser | stringLiteral | parens

  def divMul[_: P]: P[Expression] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(chain _ tupled)

  def addSub[_: P]: P[Expression] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map(chain _ tupled)

  def lessGreater[_: P]: P[Expression] = P(addSub ~ (("<=" | ">=" | "==" | "<" | ">").! ~/ addSub).rep).map(chain _ tupled)

  def expr[_: P]: P[Expression] = lessGreater

  def chain(initial: Expression, rest: Seq[(String, Expression)]): Expression = {
    rest.foldLeft(initial) {
      case (left, (operator, right)) => {
        operator match {
          case "*" => ABinary(Multiply, left, right)
          case "/" => ABinary(Divide, left, right)
          case "+" => ABinary(Add, left, right)
          case "-" => ABinary(Subtract, left, right)
          case "<" => RBinary(Less, left, right)
          case ">" => RBinary(Greater, left, right)
          case "==" => RBinary(Equal, left, right)
          case ">=" => RBinary(GreaterEqual, left, right)
          case "<=" => RBinary(LessEqual, left, right)
        }
      }
    }
  }

  def expressionParser[_: P]: P[Expression] = {
    expr.rep(min=1, sep = ".").map(expressions => {
      expressions.length match {
        case 0 => BlockExpr(Seq())
        case 1 => expressions.head
        case _ => NestedExpr(expressions)
      }
    })
  }

  def arrayParser[_: P]: P[ArrayValue] = P("[" ~ expressionParser.rep(sep=",") ~ "]").map(ArrayValue)

  def identifierParser[_: P]: P[Identifier] = LexicalParser.identifier.map(x => Identifier(Name(x)))

  def booleanParser[_: P]: P[BoolConst] = LexicalParser.booleanConst.map(BoolConst)

  def finalParser[_: P]: P[Final.type] = P("final").map(_ => Final)

  def methodCallParser[_: P]: P[MethodCall] = P(nameParser ~ "(" ~/ expressionParser.rep(sep = ",") ~ ")").map(x => MethodCall(x._1, x._2))

  def nameParser[_: P]: P[Name] = LexicalParser.identifier.map(x => Name(x))

  def newClassInstanceParser[_: P]: P[NewClassInstance] = P("new" ~/ typeRefParser ~ "(" ~ expressionParser.rep(sep = ",") ~ ")").map(x => NewClassInstance(x._1, x._2, None))

  def numberParser[_: P]: P[Expression] = P(LexicalParser.floatNumber ~ P("F" | "f")).map(FloatConst) | P(LexicalParser.longInteger).map(LongConst) | P(LexicalParser.floatNumber).map(DoubleConst) | P(LexicalParser.integer).map(IntConst)

  def stringLiteral[_: P]: P[StringLiteral] = LexicalParser.stringLiteral.map(x => StringLiteral(x))

  def ternaryParser[_: P]: P[Ternary] = P(expressionParser ~ "?" ~ expressionParser ~ ":" ~ expressionParser).map(x => Ternary(x._1, x._2, x._3))

  def typeRefParser[_: P]: P[Type] = refParser.map(Type)

  def refParser[_: P]: P[Ref] = P(nameParser.rep(sep = ".", min = 2)).map(x => RefQual(QualName(Namespace(x.dropRight(1)), x.last))) | P(nameParser).map(RefLocal)


  def op[_: P](s: P0, rhs: Operator): P[Operator] = s.!.map(_ => rhs)

  def Lt[_: P]: P[Operator] = op("<", Less.asInstanceOf[Operator])

  def Gt[_: P]: P[Operator] = op(">", Greater.asInstanceOf[Operator])

  def Eq[_: P]: P[Operator] = op("==", Equal.asInstanceOf[Operator])

  def GtE[_: P]: P[Operator] = op(">=", GreaterEqual.asInstanceOf[Operator])

  def LtE[_: P]: P[Operator] = op("<=", LessEqual.asInstanceOf[Operator])

  def comp_op[_: P]: P[Operator] = P(LtE | GtE | Eq | Gt | Lt)

  def add[_: P]: P[Operator] = op("+", Add.asInstanceOf[Operator])

  def subtract[_: P]: P[Operator] = op("-", Subtract.asInstanceOf[Operator])

  def multiply[_: P]: P[Operator] = op("*", Multiply.asInstanceOf[Operator])

  def divide[_: P]: P[Operator] = op("/", Divide.asInstanceOf[Operator])

  def and[_: P]: P[Operator] = op("&&", And.asInstanceOf[Operator])

  def or[_: P]: P[Operator] = op("||", Or.asInstanceOf[Operator])
}
