package cobalt.integration

import java.nio.file.Paths

import cobalt.utils.CompilerUtil
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, Matchers}

@RunWith(classOf[JUnitRunner])
class IfStatementCompileTest extends FunSpec with Matchers {

  describe("If Statement compile test") {
    it("Should compile if statements") {

      val output: Array[String] = CompilerUtil.executeJava(Paths.get("integration/IfStatementTest"))
      output(0) shouldBe "1"
      output(1) shouldBe "2"
      output(2) shouldBe "2"
    }
  }

}
