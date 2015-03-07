{-# OPTIONS_GHC -Wall #-}
module Calc where

import ExprT
import Parser


eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr string = case parseExp Lit Add Mul string of
	Just expression -> Just (eval expression)
	Nothing -> Nothing

