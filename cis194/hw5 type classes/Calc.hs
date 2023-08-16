{-# OPTIONS_GHC -Wall -Werror #-}

module Calc where

import ExprT
import Parser

-- Ex. 1
eval :: ExprT -> Integer

eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- Ex. 2
evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp Lit Add Mul s

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit  = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id