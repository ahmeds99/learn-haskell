{-# OPTIONS_GHC -Wall -Werror #-}

-- Ex. 1

fib :: Integer -> Integer
fib n | n <= 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]