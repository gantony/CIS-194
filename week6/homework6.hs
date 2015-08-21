{-# OPTIONS_GHC -Wall #-}


fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n =  fib (n-1) + fib (n-2)


fibs1 :: [Integer]
fibs1 = map fib [0..]


-- https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-List.html
-- http://zvon.org/other/haskell/Outputprelude/scanl_f.html
fibs2 :: [Integer] 
fibs2 = 0 : scanl (+) 1 fibs2