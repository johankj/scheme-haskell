module LispVal where

import Environment
import Helpers
import LispError

import Data.Char
import Data.Ratio (Rational (..), (%))
import Data.Complex (Complex (..))
import Data.IORef
import System.IO (Handle)

type LispEnv = Environment LispVal

type ThrowsLispError a = ThrowsError LispVal a
type IOThrowsLispError a = IOThrowsError LispVal a

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
             | PrimitiveFunc ([LispVal] -> ThrowsLispError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal], closure :: LispEnv }
             | IOFunc ([LispVal] -> IOThrowsLispError LispVal)
             | Port Handle -- port/handle is basically a file descriptor

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func { params = args, vararg = vararg, body = body, closure = env }) =
    "(lambda (" ++ unwords (map show args) ++
      (case vararg of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"

