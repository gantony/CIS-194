{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List


-- Exercise 1

keepValue :: (a, Int) -> Int -> Bool
keepValue (_, i) n = (i `mod` n) /= 0

keepEvery :: [a] -> Int -> [a]
keepEvery list n = foldl (\ acc (i, elm) -> if (elm `mod` n) /= 0 then acc else acc ++ [i]) [] (zip list [1..]) 
--keepEvery list n = fst (unzip (filter (\ x -> keepValue x n ) (zip list [1..]))) --foldl (\ acc (i, elm) -> if (elm `mod` n) /= 0 then acc else acc ++ [i]) [] (zip list [1..]) 

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

data Histogram = Histogram Integer Integer Integer Integer Integer Integer Integer Integer Integer Integer
               deriving Show

numOccurences :: [Integer] -> Integer -> Integer
numOccurences list x = toInteger $ length $ elemIndices x list

numElements :: [Integer] -> [Integer]
numElements list = [n 0, n 1, n 0, n 3, n 4, n 5, n 6, n 7, n 8, n 9]
                 where n x = numOccurences list x

toChars :: Integer -> String
toChars n = take (fromIntegral  n) $ repeat '*'

elemStrings :: [Integer] -> [String]
elemStrings list = map toChars (numElements list)

fullElemStrings :: [String] -> Integer -> [String]
fullElemStrings strings m = map (\ s -> (take (fromIntegral (m - (fromIntegral (length s)))) $ repeat ' ') ++ s) strings 

getMax :: [String] -> Int
getMax strings = maximum $ map length strings

hozLines :: [Integer] -> [String]
hozLines list = fullElemStrings strings ( fromIntegral (getMax strings))
              where strings = elemStrings list

vertLines :: [Integer] -> [String]
vertLines list = transpose $ hozLines list

vertLinesBroken :: [Integer] -> [String]
vertLinesBroken list = map (\s -> s ++ "\n") (transpose $ hozLines list)

histogram :: [Integer] -> String
histogram list = concat $ (vertLinesBroken list) ++ ["==========\n"] ++ ["0123456789\n"]
