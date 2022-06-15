{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

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

data ToOutput where
  Num :: Char -> ToOutput
  Add :: ToOutput -> ToOutput -> ToOutput

toParseTree :: ToOutput -> Exp Symbol
toParseTree (Num c) = C c
toParseTree (Add l r) = Cons (C '(') (Cons (toParseTree l) (Cons (C '+') (Cons (toParseTree r) (C ')'))) )

toAST :: ToOutput -> AST
toAST (Num c) = ANum c
toAST (Add l r) = AAdd (toAST l) (toAST r)

-- parse rules

expression :: Parser ToOutput
expression = numtoken <|> sumExpression

numtoken :: Parser ToOutput
numtoken = Num <$> (char '1' <|> char '2' <|> char '3')

sumExpression :: Parser ToOutput
sumExpression = do
  string "("
  l <- expression
  string "+"
  r <- expression
  string ")"
  return $ Add l r
