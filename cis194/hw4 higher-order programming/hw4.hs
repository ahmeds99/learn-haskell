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


-- Ex. 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

insertNode :: a -> Tree a -> Tree a
insertNode a Leaf = Node 0 Leaf a Leaf
insertNode a (Node h left x right)
    | height1 < height2 = Node h                           (insertNode a left) x right
    | height1 > height2 = Node h                           left x (insertNode a right)
    | otherwise         = Node (1 + max (heightOfTree left) heightInserted) left x (insertNode a right)
    where 
        height1 = heightOfTree left
        height2 = heightOfTree right
        inserted = insertNode a right
        heightInserted = heightOfTree inserted
        

heightOfTree :: Tree a -> Integer
heightOfTree Leaf = -1
heightOfTree (Node height _ _ _) = height

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

-- From: https://stackoverflow.com/questions/16157203/build-balanced-binary-tree-with-foldr
showTree :: Show a => Tree a -> String
showTree Leaf = ""  
showTree n@(Node i _ _ _) = go i n
  where
  go _ Leaf = "" 
  go i (Node _ l c r) = go (i-1) l ++ 
    replicate (4*fromIntegral i) ' ' ++ show c ++ "\n" ++ go (i-1) r 

-- Ex. 3: more folds
-- 1. xor

xor :: [Bool] -> Bool
xor = foldl (/=) False

-- 2. Map as a fold 
map' :: (a -> b) -> [a] -> [b]
-- Handle the : (head and rest of list) by applying f to the head, and append the rest of the list to head
-- Handles the empty case of a list simply as an empty list, as map f [] == []
map' f = foldr (\x xs -> f x : xs) []