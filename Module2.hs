module Module2 where
import Data.Function
--import Data.Tuple

{-task1-}
getSecondFrom :: t -> t2 -> t1 -> t2
getSecondFrom z x c = x 

{-task2-}
--multSecond = g `on` h
--g = (*)
--h (x,y) = y 

{-task3-}
--on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
--on3 op f x y z = op a b c where a = f x
--                                b = f y
--                                c = f z

{-task4-}
doItYourself = f . g . h
f = logBase 2
g = (^ 3)
h = max 42

{-test-}
swap :: (a,b) -> (b,a)
swap = uncurry (flip (,))

{-task 5 and 6 -}
class Printable a where
    toString :: a -> [Char]

instance Printable Bool where
  toString True = "true"
  toString False = "false"
 
instance Printable () where
   toString _ = "unit type"

instance (Printable a,Printable b) => Printable (a,b) where
   toString pair = "(" ++ (toString $ fst pair) ++ "," ++ (toString $ snd pair) ++ ")"

{-task7-}
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x
       | doesEnrageMork x = stomp x
       | doesEnrageGork x = stab x 
       | doesEnrageGork x && doesEnrageMork x = stomp (stab x)
       | otherwise = x

{-task8-}
newtype AddDotAtEnd = AddDotAtEnd Int

instance Show AddDotAtEnd where
  show (AddDotAtEnd s) = show s ++ "."
a = AddDotAtEnd 127
b = AddDotAtEnd 224
c = AddDotAtEnd 120
d = 12

{-task9-}
class (Enum a,Bounded a,Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | x == maxBound = minBound
    | otherwise = succ x

  spred :: a -> a
  spred x
    | x == minBound = maxBound
    | otherwise = pred x

{-task10-}
avg :: Int -> Int -> Int -> Double
avg x y z = (fromIntegral x + fromIntegral y + fromIntegral z) / 3

{-task10 sum-}
avg2 :: Int -> Int -> Int -> Double
avg2 x y z = (/ 3) $ sum [i,j,k]
    where
        i = fromIntegral x
        j = fromIntegral y
        k = fromIntegral z