package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST.{Abstract, Final, PackageLocal, Private, Protected, Pure}

class ModifierParserTest extends AnyFunSpec with Matchers {
  describe("Modifier parser") {

    it("Should parse protected modifier") {
      val Parsed.Success(value, _) = parse("protected", StatementParser.accessModifier(_))
      value shouldBe Protected
    }
    it("Should parse private modifier") {
      val Parsed.Success(value, _) = parse("private", StatementParser.accessModifier(_))
      value shouldBe Private
    }
    it("Should parse package local modifier") {
      val Parsed.Success(value, _) = parse("local", StatementParser.accessModifier(_))
      value shouldBe PackageLocal
    }
    it("Should parse final modifier") {
      val Parsed.Success(value, _) = parse("final", StatementParser.accessModifier(_))
      value shouldBe Final
    }
    it("Should parse abstract modifier") {
      val Parsed.Success(value, _) = parse("abstract", StatementParser.typeModifier(_))
      value shouldBe Abstract
    }
    it("Should parse pure modifier") {
      val Parsed.Success(value, _) = parse("pure", StatementParser.typeModifier(_))
      value shouldBe Pure
    }
  }
}
