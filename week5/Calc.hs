{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser


eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

-- https://wiki.haskell.org/Maybe
evalStr :: String -> Maybe Integer
evalStr string = case parseExp Lit Add Mul string of
	Just expression -> Just (eval expression)
	Nothing -> Nothing

reify :: ExprT -> ExprT
reify = id

class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul :: a -> a -> a

instance Expr ExprT where
	lit x = (Lit x)
	add a b = (Add a b)
	mul a b = (Mul a b)

instance Expr Integer where
	lit x = x
	add a b = a + b
	mul a b = a * b 

instance Expr Bool where
	lit x
		| x > 0 = True
		| otherwise = False
	add a b = a || b
	mul a b = a && b 

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
	lit x = (MinMax x)
	add (MinMax a) (MinMax b) = MinMax (max a b)
	mul (MinMax a) (MinMax b) = MinMax (min a b) 

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

-- Preventing future duplication :)
mod7 :: Integer -> Mod7
mod7 = Mod7 . mod 7 	-- same as mod7 x = Mod7 (mod 7 x)

instance Expr Mod7 where	
	lit x = mod7 x
	add (Mod7 a) (Mod7 b) = mod7 $ a + b
	mul (Mod7 a) (Mod7 b) = mod7 $ a * b 

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
