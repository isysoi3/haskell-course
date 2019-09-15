elemX n [] = False
elemX n (x:xs) = if (n == x) then True else elemX n xs

(<+>) :: [a] -> [a] -> [a]
(<+>) [] ys = ys
(<+>) (x:xs) ys = x: (xs <+> ys)

--reverse +
reverseX_helper [] ns = ns
reverseX_helper (x:xs) ns = reverseX_helper xs (x:ns)
reverseX xs = reverseX_helper xs []

--sum +
sumX_helper [] r = r
sumX_helper (x:xs) r = sumX_helper xs x+r
sumX xs = sumX_helper xs 0

--last +
lastX (x:xs) = if (xs == []) then x else lastX xs

--init +
initX_helper (x:xs) ns = if (xs == []) then ns else initX_helper xs (x:ns)
initX xs = reverseX (initX_helper xs [])

--map + 
mapX_helper _ [] ns = ns
mapX_helper f (x:xs) ns = (f x):mapX_helper f xs ns 
mapX f xs = mapX_helper f xs []

--take + 
takeX_helper 0 _ ns = ns
takeX_helper _ [] ns = ns
takeX_helper c (x:xs) ns = takeX_helper (c-1) xs (x:ns)
-- takeX :: [Int] -> [a] -> [a]
takeX c xs = reverseX (takeX_helper c xs [])

-- inits(*) +
initsX_helper [] = []
initsX_helper xs = (initX xs) : initsX_helper (initX xs)
initsX xs = reverseX (xs : initsX_helper xs)

-- tails(*) +
tailsX_helper [] = []
tailsX_helper (_:xs) = xs:tailsX_helper xs
tailsX xs = xs:tailsX_helper xs

-- список всех подможеств