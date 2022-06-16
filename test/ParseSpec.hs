module ParseSpec (spec) where

import Debug.Trace
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Text.Parsec (parse)

import HLispGen.Grammar (repr, Exp)
import HLispGen.Language (expression, Symbol, toParseTree)


prop_parse_randomly_generated_trees :: Exp Symbol -> Bool
prop_parse_randomly_generated_trees exp = case parse expression "" (repr exp) of
  Left err -> False
  Right res -> trace (show exp) $ trace (show $ toParseTree res) $ exp == toParseTree res


spec :: Spec
spec = do
  describe "foo" $ do
    it "can parse randomly generated trees" $ property prop_parse_randomly_generated_trees
