{-# LANGUAGE FlexibleInstances #-}
module HLispGen.Grammar where

-- tools for defining a language specification and automatically generating test data

import Data.List(intercalate)
import Test.QuickCheck ( oneof, Arbitrary(..), Gen )

data Exp a = C Char
           | Cons (Exp a) (Exp a)
           | Option [Exp a]
           | I a
           deriving (Eq, Show)

foldExp :: (Char -> r) -> (r -> r -> r) -> ([r] -> r) -> (a -> r) -> Exp a -> r
foldExp char cons option iFunc exp = case exp of
  C c         -> char c
  Cons l r    -> cons (foldExp char cons option iFunc l) (foldExp char cons option iFunc r)
  Option opts -> option (map (foldExp char cons option iFunc) opts)
  I i         -> iFunc i

repr :: (Show a) => Exp a -> String
repr = foldExp char cons option iFunc
  where
    char c      = [c]
    cons l r    = l ++ r
    option opts = "(" ++ intercalate "|" opts ++ ")"
    iFunc s     = "<" ++ show s ++ ">"

class Rhs a where
  headSymbol :: a
  rhs        :: a -> Exp a

instance (Rhs a) => Arbitrary (Exp a) where
  arbitrary = foldExp char cons option iFunc (I $ return headSymbol)
    where
      char c      = return $ C c
      cons l r    = do
        l' <- l
        r' <- r
        return $ Cons l' r'
      option opts = oneof opts
      iFunc i     = rhs <$> i
  shrink _ = []
