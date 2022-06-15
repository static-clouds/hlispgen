{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module HLispGen.Lib where

import Data.List(intercalate)
import Test.QuickCheck ( oneof, Arbitrary(..), Gen )

-- TODO: Have different types for parse rules and parse trees
data Exp a = C Char
           | Cons (Exp a) (Exp a)
           | Option [Exp a]
           | I a
           deriving (Eq, Show)

foldExp :: (Char -> r) -> (Exp a -> Exp a -> r) -> ([Exp a] -> r) -> (a -> r) -> Exp a -> r
foldExp char cons option iFunc exp = case exp of
  C c         -> char c
  Cons l r    -> cons l r
  Option opts -> option opts
  I i         -> iFunc i

repr :: (Show a) => Exp a -> String
repr = go
  where
    go          = foldExp char cons option iFunc
    char c      = [c]
    cons l r    = go l ++ go r
    option opts = "(" ++ intercalate "|" (map go opts) ++ ")"
    iFunc s     = "<" ++ show s ++ ">"

data Symbol = Token | NumToken | Plus | LParen | RParen deriving (Eq, Show)

class Rhs a where
  headSymbol :: a
  rhs        :: a -> Exp a

instance Rhs Symbol where
  headSymbol = Token
  rhs Token    = Option [ I NumToken
                        , Cons (I LParen) (Cons (I Token) (Cons (I Plus) (Cons (I Token) (I RParen))) )
                        ]
  rhs NumToken = Option [C '1', C '2', C '3']
  rhs Plus     = C '+'
  rhs LParen   = C '('
  rhs RParen   = C ')'


instance (Rhs a) => Arbitrary (Exp a) where
  arbitrary = go (I headSymbol)
    where
      go :: (Rhs a) => Exp a -> Gen (Exp a)
      go          = foldExp char cons option iFunc
      char c      = return $ C c
      cons l r    = do
        l' <- go l
        r' <- go r
        return $ Cons l' r'
      option opts = oneof $ map go opts
      iFunc i     = (go . rhs) i
  shrink _ = []
