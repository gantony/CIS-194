
-- Instructions: http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

import Test.HUnit


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


-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits list = sum . concat $ map toDigits list

{- 
    Difference between . and $ : http://stackoverflow.com/a/1290727/1412348
    $ : anything appearing after it will take precedence over anything that comes before
    Example: f $ a + b == f (a + b)
    . : used to chain function. Let you tie the output of whatever appears on the right to the input of whatever appears on the left
    In other words in a chain of $s, all but the final one can be replaced by .
-}


-- Exercise 4

validate :: Integer -> Bool
validate n = mod (sumDigits . doubleEveryOther $ toDigits n) 10 == 0

-- Tests

testCharToString = TestCase $ assertEqual 
    "return String representation of given Char" "1" (charToString '1')

testCharToInt = TestCase $ assertEqual 
    "return Integer representation of given Char" 1 (charToInt '1')

testToDigits = TestList [
    TestCase $ assertEqual 
        "return list of individual Integers representating a given number" [1,2,3,4] (toDigits 1234),
    TestCase $ assertEqual 
        "return [] if 0" [] (toDigits 0),
    TestCase $ assertEqual 
        "return [] if <0" [] (toDigits (-17))
    ]

testDoubleEveryOther = TestList [
    TestCase $ assertEqual 
        "doubles every other numbers, starting from the right, with an even number of numbers" [16,7,12,5] (doubleEveryOther  [8,7,6,5]),
    TestCase $ assertEqual 
        "doubles every other numbers, starting from the right, with an odd number of numbers" [1,4,3] (doubleEveryOther [1,2,3])
    ]

testSumDigits = TestList [
    TestCase $ assertEqual 
        "return the sum of individual digits in list" 6 (sumDigits [1,2,3]),
    TestCase $ assertEqual 
        "also sums individual digits in numbers in list" 15 (sumDigits [1,2,3, 45]),
    TestCase $ assertEqual 
        "is generic, sums individual digits in number made up of more than 2 digits" 10 (sumDigits [1,111,111111])
    ]

testValidate = TestList [
    TestCase $ assertEqual 
        "returns true if the number is valid" True (validate  4012888888881881),
    TestCase $ assertEqual
        "returns false if the number is invalid" False (validate 4012888888881882)
    ]

creditCardTests = TestList [testCharToString, testCharToInt, testToDigits, testDoubleEveryOther, testSumDigits, testValidate] 


main = runTestTT $ TestList [creditCardTests]
