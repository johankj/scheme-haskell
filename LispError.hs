module LispError where

import LispVal

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

-- ErrorTypes
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

-- Make LispError instance of Error
-- so it works with GHC's built-in error handling functions
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

-- Curried TypeAlias
-- e.g. ThrowsError LispVal or ThrowsError String
type ThrowsError = Either LispError

-- We'll be converting all of our errors to their string representations
-- and returning that as a normal value.
trapError action = catchError action (return . show)


