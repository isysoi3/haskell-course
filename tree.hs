-- Tree, node count, insert in tree< Ord, search 
data Tree t = Empty
            | Nonempty t (Tree t) (Tree t)

countT :: Tree t -> Int
countT Empty = 0
countT (Nonempty _ l r) = 1 + (countT l) + (countT r)

insert :: (Ord a) => Tree a -> a -> Tree a 
insert Empty v = Nonempty v Empty Empty
insert q@(Nonempty p l r) v = if (v == p) then q else if (v < p) then Nonempty p (insert l v) r else Nonempty p l (insert r v) 

search :: (Ord a) => Tree a -> a -> Bool
search Empty _ = False
-- search (Nonempty p l r) n = if (n == p) then True else if (n < p) then search l n else search r n
search (Nonempty p l r) n = (n == p) || ((n < p) && (search l n)) || (search r n)


toList :: Tree a -> [a]
toList Empty = []
toList (Nonempty p l r) = (toList l) ++ [p] ++ (toList r)
--toList (Nonempty p l r) = (toList l) ++ (p:(toList r))

fromList :: Ord a => [a] -> Tree a
fromList [] = Empty
fromList (x:xs) = insert (fromList xs) x    
-- fromList xs = foldr (flip insert) Empty xs

toString :: Show t => Tree t -> String
toString Empty = "."
toString (Nonempty p l r) = "<" ++ (toString l) ++ "|" ++ (show p) ++ "|" ++ (toString r) ++ ">" 

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Empty = Empty
mapTree f (Nonempty p l r) = Nonempty (f p) (mapTree f l) (mapTree f r)

instance Show a => Show (Tree a) where
    show = toString

instance Functor Tree where 
    fmap = mapTree

-- Homework
-- Array (key value)
-- [(key, value)]
-- insert value with key
-- find value with key -> Maybe