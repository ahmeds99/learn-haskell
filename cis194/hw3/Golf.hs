module Golf where

-- Ex. 1: Hopscotch
-- Make sure every element is the same as the first
duplicateFirst x y = x

-- From https://stackoverflow.com/questions/2026912/how-to-get-every-nth-element-of-an-infinite-list-in-haskell
-- To only get every nth element of a list
every n xs = case drop (n-1) xs of
              y : ys -> y : every n ys
              [] -> []

-- Takes in a tuple of (index, [a]) and gets every n element of a
nthElement (n, ls) = every n ls

-- Gives us the index with the list, 1-indexed, to allow for every nth item to get extracted
toIndexTuple :: [b] -> [(Int, b)]
toIndexTuple = zip[1..]

-- Maps everything to the first element, gets the indicies, and extracts only the nth elements
skips :: [a] -> [[a]]
skips [] = []
skips xs = map nthElement (toIndexTuple (map (duplicateFirst xs) xs))
