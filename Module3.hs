module Module3 where
import Data.List
import Data.Char
import Prelude

{-task1-}
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y c = x : y : c

{-task2-}
nTimes :: a -> Int -> [a]
nTimes x n
    | n == 0 = []
    | otherwise = x : nTimes x (n - 1)
   
{-task3-}
oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs)
  | odd x = x : oddsOnly xs
  | otherwise = oddsOnly xs

{-task3 elegant-}
oddsOnly2 = filter odd

{-task4-}
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == last xs) && (isPalindrome (init xs))

{-task5-}
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 [] ys zs = sum3 [0] ys zs
sum3 xs [] zs = sum3 xs [0] zs
sum3 xs ys [] = sum3 xs ys [0]
sum3 (x:xs) (y:ys) (z:zs) = x+y+z : sum3 xs ys zs

{-task6-}
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = helper xs [x] where
    helper [] acc = [acc]
    helper (e:es) acc   | e == head acc = helper es (e:acc)
                        | otherwise = acc : helper es [e]

{-task7-} 
readDigits :: String -> (String, String)
readDigits x = span isDigit x

{-task8-}
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj a b = filter (\x -> a x || b x)

{-task9-}
qsort :: Ord a => [a] -> [a]
qsort x = sort x

{-task10-}
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x*x,x*x*x])

{-task11-}
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = let
    len = length xs
    xperm p n = let
        (l, r) = splitAt n p
      in l ++ [x] ++ r
    xperms p = map (xperm p) [0..len]
  in concatMap xperms $ perms xs
