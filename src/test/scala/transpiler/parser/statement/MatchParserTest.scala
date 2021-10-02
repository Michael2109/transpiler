package transpiler.parser.statement

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
class MatchParserTest extends AnyFunSpec with Matchers {
  /* TODO
      let codeMatch = unlines [ "match obj with"
                            , "    ClassName1 -> i"
                            , "    ClassName2 -> j"
                            , "    (_)        -> k"
                            ]
   */

  /* TODO
      let codeMatchDoBlock = unlines [ "match obj with"
                                   , "    ClassName1 -> do"
                                   , "        i"
                                   , "        j"
                                   , "    ClassName2 -> j"
                                   , "    (_)        -> do"
                                   , "        k"
                                   , "        z"
                                   ]
   */
}
