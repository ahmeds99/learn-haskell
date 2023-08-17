{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Calc where

import ExprT
import Parser
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)

-- Ex. 1
eval :: ExprT -> Integer

eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- Ex. 2
evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp ExprT.Lit ExprT.Add ExprT.Mul s

-- Ex. 3
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

-- Ex. 4
instance Expr Integer where
    lit a = a
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit b = b > 0
    add a b = a || b
    mul a b = a && b

-- From the course page
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit a = Mod7 (a `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-- Ex. 6
class HasVars a where
    var :: String -> a

data VarExprT = VarExprT String Integer
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = VarExprT ""
    add (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a + b)
    mul (VarExprT _ a) (VarExprT _ b) = VarExprT "" (a * b)

instance HasVars VarExprT where
    var s = VarExprT s 0

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- Looked a bit on a solution for this unfortonuately, from here: https://github.com/bschwb/cis194-solutions/blob/main/05-typeclasses/Calc.hs
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit i _ = Just i

    add a b m = if isNothing (a m) || isNothing (b m)
                then Nothing
                else Just (fromJust (a m) + fromJust (b m))

    mul a b m = if isNothing (a m) || isNothing (b m)
                then Nothing
                else Just (fromJust (a m) * fromJust (b m))

-- From course page
withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs e = e $ M.fromList vs

