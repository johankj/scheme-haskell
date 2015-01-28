module LispError where

import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)

-- ErrorTypes
data LispError a = NumArgs Integer [a]
                 | TypeMismatch String a
                 | Parser ParseError
                 | BadSpecialForm String a
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String

instance (Show a) => Show (LispError a) where show = showError

showError :: (Show a) => LispError a -> String
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
instance Error (LispError a) where
    noMsg = Default "An error has occurred"
    strMsg = Default

-- Curried TypeAlias
-- e.g. ThrowsLispError LispVal or ThrowsLispError String
type ThrowsError a b = Either (LispError a) b
type IOThrowsError a b = ErrorT (LispError a) IO b

-- We'll be converting all of our errors to their string representations
-- and returning that as a normal value.
trapError action = catchError action (return . show)

-- Some monadic error-handling
liftThrows :: ThrowsError a b -> IOThrowsError a b
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: (Show a) => IOThrowsError a String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

extractValue :: ThrowsError a b -> b
extractValue (Right val) = val



