package transpiler.parser.statement

import transpiler.parser.ast.AST.{Annotation, Name}
import transpiler.parser.ExpressionParser
import transpiler.utils.TestUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class AnnotationParserTest extends FunSpec with Matchers
{

  describe("Annotation parser"){

    it("Annotation lower case"){
      TestUtil.parse("@annotation", ExpressionParser.annotationParser) shouldBe Annotation(Name("annotation"))
    }

    it("Annotation upper case"){
      TestUtil.parse("@ANNOTATION", ExpressionParser.annotationParser) shouldBe Annotation(Name("ANNOTATION"))
    }

    it("Annotation mixed"){
      TestUtil.parse("@Annotation", ExpressionParser.annotationParser) shouldBe Annotation(Name("Annotation"))
    }

  }

}
