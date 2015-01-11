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

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)


-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

addNode :: Tree a -> a -> Tree a
addNode Leaf a = Node 0 Leaf a Leaf
addNode (Node _ Leaf x Leaf) y = (Node 1 (Node 0 Leaf y Leaf) x Leaf)
addNode (Node n (Node l lt1 ltv lt2) x Leaf) y = (Node n (Node l lt1 ltv lt2) x (Node 0 Leaf y Leaf))
addNode (Node n Leaf x (Node r rt1 rtv rt2)) y = (Node n (Node 0 Leaf y Leaf) x (Node r rt1 rtv rt2))
addNode (Node n (Node l lt1 ltv lt2) x (Node r rt1 rtv rt2)) y 
    | l < r     = (Node (n+1) (addNode (Node l lt1 ltv lt2) y) x (Node r rt1 rtv rt2)) 
    | otherwise = (Node (n+1) (Node l lt1 ltv lt2) x (addNode (Node r rt1 rtv rt2) y)) 

foldTree :: [a] -> Tree a
foldTree list = foldl addNode Leaf (reverse list)

--depth :: Tree a -> Integer 
--depth Leaf _ = 0
--depth (Node d _ _ _) _ = d

--insert :: Tree a -> a -> Tree a
--insert Leaf a = Node 0 Leaf a Leaf
--insert tree a = 


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
