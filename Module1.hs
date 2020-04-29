module Module1 where
import Data.Char
{-task1-}
sumVec3 x y c = sqrt(x ^ 2 + y ^ 2 + c ^ 2)

{-task2-}
sign x = if x > 0 then 1 else if x == 0 then 0 else -1 

{-task3-}
x |-| y = abs(x - y)

{-task4-}
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum
standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

{-task5-}
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if (isDigit x && isDigit y) 
    then digitToInt x * 10 + digitToInt y
        else 100

{-task6-}
dist :: (Double, Double) -> (Double, Double) -> Double
dist x y = sqrt $ (fst y - fst x)^2 + (snd y - snd x)^2

{-task7-}
doubleFact :: Integer -> Integer
doubleFact n
   | n == 0 = 1
   | n == 1 = 1
   | n < 0 = error "arg must be >=0"
   | n > 0 = n * doubleFact (n - 2)

{-task8-}
fibonacci :: Integer -> Integer
fibonacci n
  | n == 0 = 0
  | n == 1 = 1
  | n < 0 = -(-1) ^ (-n) * fibonacci (-n)
  | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)

{-task9-}
fibonacci2 :: Integer -> Integer
fibonacci2 n = helper (0, 1) n where
    helper (x0,x1) n | n == 0 = x0
                     | n == 1 = x1
                     | n > 0 = helper (x1,x1 + x0) (n-1)
                     | n < 0 = helper (x0 - x1,x1) (n+1) 

{-task10-}
{-where-}
seqA :: Integer -> Integer
seqA n = helper 1 2 3 n where
    helper x1 x2 x3 n   | n == 0 = x1
                        | n == 1 = x2
                        | n == 2 = x3
                        | otherwise = helper x2 x3 (x3 + x2 - 2 * x1) (n-1)

{-task10-}
{-let in-}
seqB :: Integer -> Integer
seqB n = let
    helper p1 _ _ 0 = p1
    helper _ p2 _ 1 = p2
    helper _ _ p3 2 = p3
    helper p1 p2 p3 n = helper p2 p3 (p3 + p2 - 2 * p1) (n-1)
    in helper 1 2 3 n

{-task11-}
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper 0 0 (abs x)
    where
        helper 0 0 0       = (0, 1)
        helper sum count 0 = (sum, count)
        helper sum count value = helper (sum + mod value 10 ) (count + 1) (div value 10)

{-task12-}
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = let
    parts = 1000
    h = (b - a) / parts
    sum acc x 0 = acc
    sum acc x n = sum (acc + f x) (x + h) (n - 1)
    in h * (f a + f b + 2 * (sum 0 (a + h) (parts - 1))) / 2