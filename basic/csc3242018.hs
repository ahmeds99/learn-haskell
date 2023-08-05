
-- andMap :: a -> bool -> [b]
-- 4a) 
andmap :: (a -> Bool) -> [a] -> Bool

-- 4b)
andmap f lst = foldl (&&) True (map f lst)

apply x f = f x
mystery :: [Maybe a -> Bool] -> Bool -- 4c) Looking at the editor
mystery = andmap (apply Nothing)