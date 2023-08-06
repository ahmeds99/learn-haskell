-- Ex. 1: Rewrite fun1 and fun2 functions in a more idiomatic Haskell style
-- Aka wholemeal programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
-- fun2' 1 = 0
-- fun2' n = if even n then n + fun2 (n `div` 2) else fun2 (3 * n + 1)

-- After looking at solution for hints
fun2' = 
      sum 
    . filter even
    . takeWhile (/= 1) 
    . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)