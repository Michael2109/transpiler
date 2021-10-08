package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.{ExpressionParser, StatementParser}
import transpiler.parser.ast._



class AnnotationParserTest extends AnyFunSpec with Matchers {

  describe("Annotation parser") {

    it("Annotation lower case") {
      val Parsed.Success(value, _) = parse("@annotation",  StatementParser.annotationParser(_))
    value shouldBe Annotation(Name("annotation"))
    }

    it("Annotation upper case") {
      val Parsed.Success(value, _) = parse("@ANNOTATION",  StatementParser.annotationParser(_))
      value shouldBe Annotation(Name("ANNOTATION"))
    }

    it("Annotation mixed") {
      val Parsed.Success(value, _) = parse("@Annotation",  StatementParser.annotationParser(_))
      value shouldBe Annotation(Name("Annotation"))
    }

  }

}
