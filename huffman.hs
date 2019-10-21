import Data.List as L
import Data.Map as M

data HuffmanTree v = Node Int (HuffmanTree v) (HuffmanTree v)
                   | Leaf v Int

getFrequency :: HuffmanTree a -> Int
getFrequency (Node f _ _) = f
getFrequency (Leaf _ f) = f

compareTrees :: HuffmanTree a -> HuffmanTree a -> Ordering
compareTrees t1 t2 
 | (getFrequency t1) < (getFrequency t2) = LT
 | (getFrequency t1) > (getFrequency t2) = GT
 | otherwise = EQ


toString :: Show a => HuffmanTree a -> String
toString (Leaf v p) = (show v) ++ "--" ++ (show p)
toString (Node p l r) = "< " ++ (toString l) ++ " | " ++ (show p) ++ " | " ++ (toString r) ++ " >" 

instance Show a => Show (HuffmanTree a) where
    show = toString

countListFrequency :: Ord a => [a] -> [(a, Int)]
countListFrequency list = M.toList $ M.fromListWith (+) [(c, 1) | c <- list]


toLeaf :: (a, Int) -> HuffmanTree a
toLeaf (v, p) = Leaf v p

toLeafs :: Ord a => [a] -> [HuffmanTree a]
toLeafs xs = L.map toLeaf (countListFrequency xs)

sortHuffmanTrees :: Ord a => [HuffmanTree a] -> [HuffmanTree a]
sortHuffmanTrees xs = sortBy compareTrees xs
            

toHuffmanTree :: Ord a => [a] -> HuffmanTree a
toHuffmanTree str = toHuffmanTree_helper $ sortHuffmanTrees $ toLeafs str
toHuffmanTree_helper (t:[]) = t
toHuffmanTree_helper (t1:t2:xs) = case compareTrees t1 t2 of
    GT -> toHuffmanTree_helper (sortHuffmanTrees (((Node (getFrequency t1 + getFrequency t2) t2 t1)):xs))
    otherwise -> toHuffmanTree_helper (sortHuffmanTrees (((Node (getFrequency t1 + getFrequency t2) t1 t2)):xs))
    
fromHuffmanTreeToMap :: Ord a => HuffmanTree a -> [(a, [Bool])]
fromHuffmanTreeToMap_helper xs (Leaf v p) = [(v, (reverse xs))]
fromHuffmanTreeToMap_helper xs (Node p l r) = (fromHuffmanTreeToMap_helper (True:xs) l) ++ (fromHuffmanTreeToMap_helper (False:xs) r)
fromHuffmanTreeToMap = fromHuffmanTreeToMap_helper []

encode :: [a] -> [Bool]
encode str =  toHuffmanTree str
map char : Int 