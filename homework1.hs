
-- Instructions: http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf


-- Exercise 1

charToString :: Char -> String
charToString c = c:[]

charToInt :: Char -> Integer
charToInt c = read (charToString c) :: Integer

toDigits :: Integer -> [Integer]
toDigits n = if n > 0 then map charToInt (show n) else []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)


-- Exercise 2

multiplierList :: [Integer] -> [Integer]
multiplierList list = [if i `mod` 2 == 0 then 2 else 1 | i <- reverse [1..n]]
    where n = length list

multiplyPair :: (Integer, Integer) -> Integer
multiplyPair (n, m) = n*m

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = map multiplyPair (zip list (multiplierList list))
