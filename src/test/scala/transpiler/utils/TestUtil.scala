package transpiler.utils

import fastparse.core.Parsed
import fastparse.noApi.P

object TestUtil {

  def parse(text: String, parser: P[_]) = {
    parser.parse(text) match {
      case Parsed.Success(value, _) => value
      case Parsed.Failure(a, b, c) => throw new Exception("Failed parsing:" + a + ":" + b + ":" + c)
    }
  }

}
