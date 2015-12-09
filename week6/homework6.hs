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


data Stream a = Cons a (Stream a)  -- deriving (Show)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs


instance Show a => Show (Stream a) where
	show (Cons x xs) = Prelude.show . take 20 $ streamToList (Cons x xs)


streamRepeat :: a -> Stream a
streamRepeat x = (Cons x (streamRepeat x))

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = (Cons (f x) (streamMap f xs))

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = (Cons x (streamFromSeed f (f x)))


nats :: Stream Integer
nats = streamFromSeed (+1) 1

powersOf2 :: Stream Integer
powersOf2 = streamMap ((2::Integer)^) nats

--ruler :: Stream Integer
