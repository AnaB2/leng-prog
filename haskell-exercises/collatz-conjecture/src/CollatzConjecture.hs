module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz number
    | number <= 0 = Nothing  -- Devuelve `Nothing` si el número es menor o igual a 0
    | otherwise   = Just (collatzHelper number 0)

collatzHelper :: Integer -> Integer -> Integer
collatzHelper 1 count = count
collatzHelper number count
    | even number = collatzHelper (number `div` 2) (count + 1)
    | otherwise   = collatzHelper (number * 3 + 1) (count + 1)


