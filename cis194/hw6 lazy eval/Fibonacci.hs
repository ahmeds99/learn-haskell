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

-- Ex. 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- These parts (both functions underneath) were taken from solution and 
-- https://stackoverflow.com/questions/55330709/implementing-the-ruler-function-using-streaminterleave 
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) b = Cons a (interleaveStreams b as)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0 ) (streamMap (+1) ruler)

