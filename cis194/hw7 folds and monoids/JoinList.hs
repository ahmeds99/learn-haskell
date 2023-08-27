import Sized
data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Ex. 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a `mappend` tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Ex. 2
-- The part for append is stolen from the solution
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a) 
    | i == 0 = Just a
    | otherwise = Nothing
indexJ i (Append m a b)
    | i < 0 || i > size1 = Nothing
    | i < size1 = indexJ i a
    | otherwise = indexJ (i - size2) b
    where size1 = getSize . size $ m
          size2 = getSize . size . tag  $ b

dropJ :: (Sized b, Monoid b) =>
    Int -> JoinList b a -> JoinList b a

dropJ i jl@(Single _ a)
    | i <= 0 = Empty
dropJ i jl@(Append m a b)
    | i < 0 || i > size1 = jl
    | i < size1 = dropJ i a
    | otherwise = dropJ (i - size2) b
    where size1 = getSize . size $ m
          size2 = getSize . size . tag  $ b
dropJ _ _ = Empty

-- Methods to safe index and test the fast indexing (indexJ)
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

test = Append (Size 3) 
        (Append (Size 2) 
            (Single (Size 1) "hei") 
            (Single (Size 1) "hade")
        )
        (Single (Size 1) "hva")