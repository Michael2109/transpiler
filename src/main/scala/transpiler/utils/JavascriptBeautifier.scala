package transpiler.utils

;

import jdk.nashorn.internal.runtime.{Context, ErrorManager}
import jdk.nashorn.internal.runtime.options.Options

import java.io.InputStreamReader
import javax.script.{Invocable, ScriptEngineManager}
import scala.io.Source
import jdk.nashorn.api.scripting.NashornScriptEngineFactory
import javax.script.ScriptEngine

object JavascriptBeautifier {

  // my javascript beautifier of choice
  private val BEAUTIFY_JS_RESOURCE: String = "beautify.js";

  // name of beautifier function
  private val BEAUTIFY_METHOD_NAME: String = "js_beautify";

  private val engine: ScriptEngine = new NashornScriptEngineFactory().getScriptEngine("--language=es6")

  // this is needed to make self invoking function modules work
  // otherwise you won't be able to invoke your function
  engine.eval("var global = this;");

  engine.eval(Source.fromResource(BEAUTIFY_JS_RESOURCE).getLines().mkString("\n"));


  def beautify(javascriptCode: String): String = {
    engine.asInstanceOf[Invocable].invokeFunction(BEAUTIFY_METHOD_NAME, javascriptCode).asInstanceOf[String]
  }
}
