import Control.Monad
import qualified Data.Map as M

isSorted:: Ord a => [a] -> Bool
isSorted_helper _ [] = True
isSorted_helper n (x:xs) = if n <= x then isSorted_helper x xs else False
isSorted [] = True
isSorted (x:xs) = isSorted_helper x xs


isSorted' l = and (zipWith (<=) l (tail l))

-- list and another list binary operation 
-- map2 :: (a -> a -> a) -> [a] -> [a] -> [a]
-- map2 _ [] _ = []
-- map2 _ _ [] = []
-- map2 bo l1@(x:xs) l2@(y:ys) = let someF_helper n arr =  map (bo n) arr in
--     someF_helper x l2 ++ someF_helper y l1 ++ someF_helper bo xs ys

map2 op xs ys = join $ map (\f -> map f ys) (map op xs) 


data MaybeX a = NothingX
              | JustX a

fmapX :: (a -> b) -> MaybeX a -> MaybeX b
fmapX _ NothingX = NothingX
fmapX f (JustX v) =  JustX (f v)


mapSecond :: (a -> b) -> (t, a) -> (t, b)  
mapSecond f (d, x) = (d, f x)

-- instance Functor (->) p where
--     fmap = (.)

-- fmap id = id
-- fmap (f.g) = (fmap f) . (fmap g)

-- f $ x = f :

s = [1..4]