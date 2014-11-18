{-# OPTIONS_GHC -Wall #-}
module Golf where


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

