-- filter :: (a -> Bool) -> [a] -> [a]

filterMx :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterMx _ [] = return []
filterMx pred (x:xs) = do
    cond <- pred x
    ls <- filterMx pred xs
    return $ if cond then x : ls else ls

filterModThree :: Integral a => a -> Maybe Bool
filterModThree x = case mod x 3 of 
    2 -> Just False
    0 -> Just True
    _ -> Nothing


--filterMx (\ _ -> [False, True]) [0..5]