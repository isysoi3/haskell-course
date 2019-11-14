module ListUtils
(allInserts, allTransposition, insertAfterLess, insertionSort)
where 

import Control.Monad

allInserts :: a -> [a] -> [[a]]
allInserts el [] = [[el]]
allInserts el l@(x:xs) = (el:l) : (map (x:) (allInserts el xs))  

allTransposition :: [a] -> [[a]]
allTransposition [] = [[]]
allTransposition (x:xs) = join (map (allInserts x) (allTransposition xs))


insertAfterLess :: Ord a => a -> [a] ->[a]
insertAfterLess el [] = [el]
insertAfterLess el (x:xs) = if x < el then x:(insertAfterLess el xs) else el:(x:xs)


insertionSort_helper s []  = s
insertionSort_helper s (u:us)  = insertionSort_helper (insertAfterLess u s) us 

insertionSort :: Ord a => [a] -> [a]
insertionSort = insertionSort_helper []