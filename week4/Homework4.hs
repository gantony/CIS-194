{-# OPTIONS_GHC -Wall #-}
module Homework4 where

--import Data.List


-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' list = foldr (*) 1 . map (subtract 2) $ filter even list

-- import Test.QuickCheck
-- quickCheck (\xs -> fun1 xs == fun1' xs)
