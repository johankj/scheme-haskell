module Evaluate where

import LispVal
import LispError
import Primitives

import System.Environment
import Control.Monad.Error
import System.IO

-- evaluate
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
-- Quotes
eval (List [Atom "quote", val]) = return val
-- Conditionals
eval (List [Atom "if", pred, conseq, alt]) =
    do result <- eval pred
       case result of
         Bool False -> eval alt
         otherwise  -> eval conseq
eval (List (Atom "cond" : xs)) = evalCond xs
eval (List (Atom "case" : (key : xs))) = evalCase key xs
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Evaluate cond
evalCondClause :: LispVal -> LispVal -> ThrowsError LispVal -> ThrowsError LispVal
evalCondClause pred conseq alt = do
    result <- eval pred
    case result of
      Bool True  -> eval conseq
      Bool False -> alt
      otherwise -> throwError $ TypeMismatch "boolean" result

evalCond :: [LispVal] -> ThrowsError LispVal
evalCond [List [Atom "else", conseq]] = eval conseq
evalCond [List [pred, conseq]] = evalCondClause pred conseq (return $ List [])
evalCond (List [pred, conseq] : xs) = evalCondClause pred conseq (evalCond xs)
evalCond [] = throwError $ BadSpecialForm "no true clause in cond expression" (List [Atom "cond"])
evalCond badArgList = throwError $ BadSpecialForm "illegal clause in cond expression" $ head badArgList

-- Evaluate case
evalCaseClause :: LispVal -> LispVal -> LispVal -> ThrowsError LispVal -> ThrowsError LispVal
evalCaseClause key val conseq alt = do
    result <- eqv [key, val]
    case result of
      Bool True  -> eval conseq
      Bool False -> alt
      otherwise  -> throwError $ TypeMismatch "boolean" result

evalCase :: LispVal -> [LispVal] -> ThrowsError LispVal
evalCase _ [List [Atom "else", conseq]] = eval conseq
evalCase key [List [val, conseq]] = evalCaseClause key val conseq (return $ List [])
evalCase key (List [val, conseq] : xs) = evalCaseClause key val conseq (evalCase key xs)
evalCase _ badArgList = throwError $ BadSpecialForm "invalid cond clause" (badArgList !! 0)


-- Applies a function to the arguments.
-- If the function is an operator from section of the function
-- application operator, we apply it to the arguments using ($ args),
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)



