{-# LANGUAGE FlexibleInstances #-}

module HLispGen.Parse where

import HLispGen.Lib ( Symbol, Exp(C, Cons) )
import Text.Parsec

type Parser = Parsec String ()

data AST = ANum Char | Add AST AST deriving Show

class ToOutput a where
  toNumToken :: Char -> a
  toAdd :: a -> a -> a

instance ToOutput (Exp Symbol) where
  toNumToken = C
  toAdd l r = Cons (C '(') (Cons l (Cons (C '+') (Cons r (C ')'))) )

instance ToOutput AST where
  toNumToken = ANum
  toAdd = Add

expression :: (ToOutput a) => Parser a
expression = numtoken <|> sumExpression

numtoken :: (ToOutput a) => Parser a
numtoken = toNumToken <$> (char '1' <|> char '2' <|> char '3')

sumExpression :: (ToOutput a) => Parser a
sumExpression = do
  string "("
  l <- expression
  string "+"
  r <- expression
  string ")"
  return $ toAdd l r
