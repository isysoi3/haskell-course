import Control.Monad.Writer
import Control.Monad.Reader

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y = if y == 0 then Nothing else Just (div x y)

half :: Int -> Maybe Int
half x = if (mod x 2) == 0 then Just (div x 2) else Nothing

f mx my = do
    x <- mx
    y <- my
    z <- safeDiv x y
    t <- half z
    return (t+1)

-- instance Monad MaybeX where
--     return x = (JustX x)
--     (>>=) NothingX _ = NothingX
--     (>>=) (JustX x) f = (f x)

-- instance Monad ListX where
--     return x = Cons x Nil
--     (>>=) xs f = concat (fmap f xs)


type LoggingBinOp = Int -> Int -> Writer [String] Int

logPlus :: LoggingBinOp

logPlus x y = do
    tell ["Adding " ++ (show x) ++ " with " ++ (show y)]
    return (x + y)

logDiv :: LoggingBinOp
logDiv x y = do
    tell ["Divide " ++ (show x) ++ " by " ++ (show y)]
    return $ x `div` y

logMult :: LoggingBinOp
logMult x y = do
    tell ["Multiply " ++ (show x) ++ " by " ++ (show y)]
    return $ x * y

wr_demo x y z = do
    u <- logPlus x y
    v <- logDiv z 2
    w <- logMult u v
    tell ["About to generate the final result"]
    return (w+1)

-- (x, old_log) >>= f
--   (y, old_log ++ f_log) where (y, f_log) = f x

data Config = Config {
        host :: String,
        port :: Int
    }

makeUrl :: String -> Reader Config String
makeUrl path = do
    h <- asks host
    p <- asks port
    return $ "https://" ++ h ++ ":" ++ (show p) ++ "/" ++ path

-- State