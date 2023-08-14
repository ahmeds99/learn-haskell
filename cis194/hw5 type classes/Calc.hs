module Calc where

import ExprT

-- Ex. 1
eval :: ExprT -> Integer

eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b