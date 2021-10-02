package transpiler.parser.statement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.ExpressionParser
import transpiler.parser.ast.AST.{Annotation, Name}
import transpiler.utils.TestUtil


class AnnotationParserTest extends AnyFunSpec with Matchers {

  describe("Annotation parser") {

    it("Annotation lower case") {
      TestUtil.parse("@annotation", ExpressionParser.annotationParser) shouldBe Annotation(Name("annotation"))
    }

    it("Annotation upper case") {
      TestUtil.parse("@ANNOTATION", ExpressionParser.annotationParser) shouldBe Annotation(Name("ANNOTATION"))
    }

    it("Annotation mixed") {
      TestUtil.parse("@Annotation", ExpressionParser.annotationParser) shouldBe Annotation(Name("Annotation"))
    }

  }

}
