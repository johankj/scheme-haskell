module LispVal where

import Helpers

import Data.Char
import Data.Ratio (Rational (..), (%))
import Data.Complex (Complex (..))


-- The data in the program
-- It can be any Lisp value
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Int, [LispVal])
             | Number Integer
             | Ratio Rational
             | Complex (Complex Double)
             | Float Double
             | String String
             | Bool Bool
             | Character Char

instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordsList xs ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (Character c) = ['\'', c, '\'']


