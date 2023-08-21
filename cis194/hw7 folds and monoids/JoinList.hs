data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Ex. 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
-- This is a much more simple and clean solution the the first I drafted
(+++) a b = Append (tag a `mappend` tag b) a b
-- (+++) jl1@(Single m1 _) jl2@(Single m2 _) = Append (tag jl1 `mappend` tag jl2) jl1 jl2
-- (+++) jl1@(Append m1 _ _) jl2@(Single m2 _) = Append (tag jl1 `mappend` tag jl2) jl1 jl2
-- (+++) jl1@(Single m1 _) jl2@(Append m2 _ _) = Append (tag jl1 `mappend` tag jl2) jl1 jl2
-- (+++) jl1@(Append m1 _ _) jl2@(Append m2 _ _) = Append (tag jl1 `mappend` tag jl2) jl1 jl2


tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m
