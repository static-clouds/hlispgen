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

data ParseOutput where
  Num :: Char -> ParseOutput
  Add :: ParseOutput -> ParseOutput -> ParseOutput

foldParseOutput :: (Char -> r) -> (r -> r -> r) -> ParseOutput -> r
foldParseOutput num add value = case value of
  (Num c)   -> num c
  (Add l r) -> add (foldParseOutput num add l) (foldParseOutput num add r)

toParseTree :: ParseOutput -> Exp Symbol
toParseTree = foldParseOutput num add
  where
    num     = C
    add l r = Cons (C '(') (Cons l (Cons (C '+') (Cons r (C ')'))) )

toAST :: ParseOutput -> AST
toAST = foldParseOutput ANum AAdd

-- parse rules

expression :: Parser ParseOutput
expression = numtoken <|> sumExpression

numtoken :: Parser ParseOutput
numtoken = Num <$> (char '1' <|> char '2' <|> char '3')

sumExpression :: Parser ParseOutput
sumExpression = do
  string "("
  l <- expression
  string "+"
  r <- expression
  string ")"
  return $ Add l r
