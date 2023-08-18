{-# OPTIONS_GHC -Wall -Werror #-}

-- Ex. 1

fib :: Integer -> Integer
fib n | n <= 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Ex. 2
fibs2 :: [Integer]
-- https://wiki.haskell.org/The_Fibonacci_sequence#Canonical_zipWith_implementation
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Ex. 3: Streams
data Stream x = Cons x (Stream x)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList 

-- Ex. 4: more streams
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))