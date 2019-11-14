data Position = Home
              | Label String
              | Cartesian Int Int


bonus :: Position -> Int
bonus Home = 1000
bonus (Label _) = 17
bonus ((Cartesian _ _)) = 1

data List a = Empty
            | Nonempty a (List a)


lengthL :: List a -> Int
lengthL Empty = 0
lengthL (Nonempty _ xs) = 1 + (lengthL xs)  

-- Конструктор типа
-- Конструктор занчения
