package transpiler.utils

import fastparse.{Parsed, parse}
import jdk.nashorn.internal.parser.Parser
import jdk.nashorn.internal.runtime.{Context, ErrorManager, Source}
import jdk.nashorn.internal.runtime.options.Options
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import transpiler.js.{AST2JS, ModelJS, ModuleJS}
import transpiler.parser.StatementParser
import transpiler.utils.{JavaScriptCompare, JavascriptBeautifier}

object JavaScriptCompare extends Matchers {

  def checkMatch(javaScript1: String, javaScript2: String): Unit = {
    assertResult(JavascriptBeautifier.beautify(javaScript1))(JavascriptBeautifier.beautify(javaScript2))
  }

  /*
      val options = new Options("nashorn");
      options.set("anon.functions", true);
      options.set("parse.only", true);
      options.set("scripting", true);
      options.set("language", "es6")

      val errors = new ErrorManager();
      val context = new Context(options, errors, Thread.currentThread().getContextClassLoader());
      val source   =  Source.sourceFor("test", compiledCode);
      val parser = new Parser(context.getEnv(), source, errors);
      val functionNode = parser.parse();
      val block = functionNode.getBody();
      val statements = block.getStatements();*/
}
