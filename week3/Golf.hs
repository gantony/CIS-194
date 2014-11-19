{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List


-- Exercise 1

keepValue :: (a, Int) -> Int -> Bool
keepValue (_, i) n = (i `mod` n) /= 0

keepEvery :: [a] -> Int -> [a]
keepEvery list n = foldl (\ acc (i, elm) -> if (elm `mod` n) /= 0 then acc else acc ++ [i]) [] (zip list [1..]) 

skips :: [a] -> [[a]]
skips list = map (\ (_,x) -> keepEvery list x ) (zip list [1..])


-- Exercise 2

data Triplet = Triplet Integer Integer Integer
             deriving Show

triplets :: [Integer] -> [Triplet]
triplets (x:y:z:zs) = [Triplet x y z] ++ triplets (y:z:zs)
triplets _ = []

single :: Triplet -> Integer
single (Triplet _ x _) = x

isMaxima :: Triplet -> Bool
isMaxima (Triplet a b c) = b > a && b > c

localMaxima :: [Integer] -> [Integer]
localMaxima list = map single $ filter isMaxima (triplets list)


-- Exercise 3

numOccurences :: [Int] -> Int -> Int
numOccurences list x = length $ elemIndices x list

numElements :: [Int] -> [Int]
numElements list = [n 0, n 1, n 0, n 3, n 4, n 5, n 6, n 7, n 8, n 9]
                 where n = numOccurences list   -- partially applied function

mkStr :: Int -> Char -> String
mkStr n c = take n $ repeat c

hozLines :: [Int] -> [String]
hozLines list = map (\n -> mkStr (m - n) ' ' ++ mkStr n '*') (numElements list)
              where m = maximum list
                    
vertLines :: [Int] -> [String]
vertLines list = map (\s -> s ++ "\n") (transpose $ hozLines list)

histogram :: [Integer] -> String
histogram list = concat $ (vertLines intList) ++ ["==========\n0123456789\n"]
                 where intList = map fromIntegral list
