module IOHelpers where

import LispError
import LispVal
import Parse

import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

readOrThrow :: Parser a -> String -> ThrowsLispError a
readOrThrow parser input =
    case parse parser "lisp" input of
      Left err -> throwError $ Parser err
      Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)
