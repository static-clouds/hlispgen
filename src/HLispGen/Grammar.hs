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

instance Semigroup (Exp a) where
  (<>) = Cons

foldExp :: (Char -> r) -> (r -> r -> r) -> ([r] -> r) -> (a -> r) -> Exp a -> r
foldExp char cons option iFunc exp = case exp of
  C c         -> char c
  Cons l r    -> cons (foldExp char cons option iFunc l) (foldExp char cons option iFunc r)
  Option opts -> option (map (foldExp char cons option iFunc) opts)
  I i         -> iFunc i


-- a fold that expands identifiers by recursively applying the non-terminal rule
foldExp' :: (Rhs a) => (Char -> r) -> (r -> r -> r) -> ([r] -> r) -> Exp a -> r
foldExp' char cons option exp = case exp of
  C c         -> char c
  Cons l r    -> cons (foldExp' char cons option l) (foldExp' char cons option r)
  Option opts -> option (map (foldExp' char cons option) opts)
  I i         -> foldExp' char cons option (rhs i)

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
  arbitrary = go (I headSymbol)
    where
      go :: (Rhs a) => Exp a -> Gen (Exp a)
      go = foldExp' char cons option
      -- char :: (Rhs a) => Char -> Gen (Exp a)
      char c      = return $ C c
      -- cons :: (Rhs a) => Gen (Exp a) -> Gen (Exp a) -> Gen (Exp a)
      cons l r    = do
        l' <- l
        r' <- r
        return $ Cons l' r'
      -- option :: (Rhs a) => [Gen (Exp a)] -> Gen (Exp a)
      option opts = oneof opts
  shrink _ = []
