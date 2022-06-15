module TokenizeSpec (spec) where

import Debug.Trace
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)
import Text.Parsec (parse)

import HLispGen.Grammar (repr, Exp)
import HLispGen.Language (expression, Symbol)


prop_parse_randomly_generated_trees :: Exp Symbol -> Bool
prop_parse_randomly_generated_trees exp = case parse expression "" (repr exp) of
  Left err -> False
  Right res -> exp == res


spec :: Spec
spec = do
  describe "foo" $ do
    it "can parse randomly generated trees" $ property prop_parse_randomly_generated_trees
