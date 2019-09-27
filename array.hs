-- Homework
-- Array (key value)
-- [(key, value)]
-- insert value with key
-- find value with key -> Maybe

data Array k v = Empty
               | Nonempty (k, v) (Array k v)

insert :: Ord k => Array k v -> (k, v) -> Array k v
insert Empty el = Nonempty el Empty
insert (Nonempty (k, v) arr) (nK, nV) = if (k == nK) then Nonempty (k, nV) arr else Nonempty (k, v) (insert arr (nK, nV))


find :: Ord k => Array k v -> k -> Maybe v
find Empty _ = Nothing
find (Nonempty (k, v) arr) sK = if (sK == k) then (Just v) else find arr sK

toString :: (Show k, Show v) => Array k v -> String
toString Empty = "end"
toString (Nonempty (k, v) arr) = (show k) ++ " " ++ (show v) ++ " | " ++ toString arr

instance (Show k, Show v) => Show (Array k v) where
    show = toString