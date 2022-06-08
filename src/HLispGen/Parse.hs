module HLispGen.Parse where

import HLispGen.Lib
import Text.Parsec

type Parser = Parsec String ()

expression :: Parser (Exp Symbol)
expression = numtoken <|> sumExpression

numtoken :: Parser (Exp Symbol)
numtoken = C <$> (char '1' <|> char '2' <|> char '3')

sumExpression :: Parser (Exp Symbol)
sumExpression = do
  string "("
  l <- expression
  string "+"
  r <- expression
  string ")"
  return $ Cons (C '(') (Cons l (Cons (C '+') (Cons r (C ')'))) )
