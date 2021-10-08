package transpiler.parser.statement

import fastparse.{Parsed, parse}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import transpiler.parser.StatementParser
import transpiler.parser.ast.AST.{Import, Name, Package}

import scala.collection.mutable.ArrayBuffer
class PackageParserTest extends AnyFunSpec with Matchers {
  describe("Package parser") {
    it("Should parse package x") {
      val code = "package x"
      val Parsed.Success(value, _) = parse(code, StatementParser.packageParser(_))
      value shouldBe Package(ArrayBuffer(Name("x")))
    }

    it("Should parse package x.y") {
      val code = "package x.y"
      val Parsed.Success(value, _) = parse(code, StatementParser.packageParser(_))
      value shouldBe Package(ArrayBuffer(Name("x"), Name("y")))
    }

    it("Should parse package x.y.z.a.b.c") {
      val code = "package x.y.z.a.b.c"
      val Parsed.Success(value, _) = parse(code, StatementParser.packageParser(_))
      value shouldBe Package(ArrayBuffer(Name("x"), Name("y"), Name("z"), Name("a"), Name("b"), Name("c")))
    }

    it("Should parse package abc.xyz.Name") {
      val code = "package abc.xyz.Name"
      val Parsed.Success(value, _) = parse(code, StatementParser.packageParser(_))
      value shouldBe Package(ArrayBuffer(Name("abc"), Name("xyz"), Name("Name")))
    }

    it("Should parse package _abc.xy_z.Name_") {
      val code = "package _abc.xy_z.Name_"
      val Parsed.Success(value, _) = parse(code, StatementParser.packageParser(_))
      value shouldBe Package(ArrayBuffer(Name("_abc"), Name("xy_z"), Name("Name_")))
    }

    it("Should parse package a1b2c3.x1y2z3") {
      val code = "package a1b2c3.x1y2z3"
      val Parsed.Success(value, _) = parse(code, StatementParser.packageParser(_))
      value shouldBe Package(ArrayBuffer(Name("a1b2c3"), Name("x1y2z3")))
    }
  }
}
