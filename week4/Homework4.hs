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

-- Come back to fun2 later


-- Exercise 2

-- Started on another computer I think


-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (\ x acc -> if x then not acc else acc) False   -- Partial function application

-- xor [False, True, False] == True
-- xor [False, True, False, False, True] == False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x xs -> (f x):xs) []

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr ((:) . f) []    -- Same as map', but the result of f is fed as an input to (:)!

map''' :: (a -> b) -> [a] -> [b]
map''' f = foldr (\ x -> ((f x) : ) ) []    -- This works too, and could be an intermediary step to get to map'' from map', using currying

