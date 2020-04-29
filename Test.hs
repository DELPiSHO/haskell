module Test where

doubleFact :: Integer -> Integer
doubleFact n | n < 0 error "arg must be >= 0"
             | n == 0 = 1
             | n == 1 = 1
             | n > 1 n * doubleFact(n - 2)
             