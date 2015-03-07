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
	mul a b =  (Mul a b)
