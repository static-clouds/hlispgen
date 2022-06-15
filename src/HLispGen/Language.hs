-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module HLispGen.Language where

import Text.Parsec ( char, string, (<|>), Parsec )
import HLispGen.Grammar ( Rhs(headSymbol, rhs), Exp(C, Cons, Option, I) )

-- a concrete specification of a simple language and its parser

data Symbol = Token | NumToken | Plus | LParen | RParen deriving (Eq, Show)

instance Rhs Symbol where
  headSymbol = Token
  rhs Token    = Option [ I NumToken
                        , Cons (I LParen) (Cons (I Token) (Cons (I Plus) (Cons (I Token) (I RParen))) )
                        ]
  rhs NumToken = Option [C '1', C '2', C '3']
  rhs Plus     = C '+'
  rhs LParen   = C '('
  rhs RParen   = C ')'

-- parser

type Parser = Parsec String ()

-- abstract syntax tree type
data AST = ANum Char | AAdd AST AST deriving Show

class ToOutput a where
  toNumToken :: Char -> a
  toAdd      :: a -> a -> a

instance ToOutput (Exp Symbol) where
  toNumToken = C
  toAdd l r  = Cons (C '(') (Cons l (Cons (C '+') (Cons r (C ')'))) )

instance ToOutput AST where
  toNumToken = ANum
  toAdd      = AAdd

-- parse rules

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
