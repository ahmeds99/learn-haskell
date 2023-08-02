module Golf where

-- Ex. 1: Hopscotch
change a = [a]

skips :: [a] -> [[a]]
skips [] = []
skips ls = map change ls
