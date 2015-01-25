module Evaluate where

import LispVal
import LispError
import Primitives
import Environment

import System.Environment
import Control.Monad.Error
import System.IO

-- evaluate
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
-- Quotes
eval env (List [Atom "quote", val]) = return val
-- Conditionals
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise  -> eval env conseq
eval env (List (Atom "cond" : xs)) = evalCond env xs
eval env (List (Atom "case" : (key : xs))) = evalCase env key xs
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Evaluate cond
evalCondClause :: Env -> LispVal -> LispVal -> IOThrowsError LispVal -> IOThrowsError LispVal
evalCondClause env pred conseq alt = do
    result <- eval env pred
    case result of
      Bool True  -> eval env conseq
      Bool False -> alt
      otherwise -> throwError $ TypeMismatch "boolean" result

evalCond :: Env -> [LispVal] -> IOThrowsError LispVal
evalCond env [List [Atom "else", conseq]] = eval env conseq
evalCond env [List [pred, conseq]] = evalCondClause env pred conseq (return $ List [])
evalCond env (List [pred, conseq] : xs) = evalCondClause env pred conseq (evalCond env xs)
evalCond env [] = throwError $ BadSpecialForm "no true clause in cond expression" (List [Atom "cond"])
evalCond env badArgList = throwError $ BadSpecialForm "illegal clause in cond expression" $ head badArgList

-- Evaluate case
evalCaseClause :: Env -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal -> IOThrowsError LispVal
evalCaseClause env key val conseq alt = do
    result <- liftThrows $ eqv [key, val]
    case result of
      Bool True  -> eval env conseq
      Bool False -> alt
      otherwise  -> throwError $ TypeMismatch "boolean" result

evalCase :: Env -> LispVal -> [LispVal] -> IOThrowsError LispVal
evalCase env _ [List [Atom "else", conseq]] = eval env conseq
evalCase env key [List [val, conseq]] = evalCaseClause env key val conseq (return $ List [])
evalCase env key (List [val, conseq] : xs) = evalCaseClause env key val conseq (evalCase env key xs)
evalCase env _ badArgList = throwError $ BadSpecialForm "invalid cond clause" (badArgList !! 0)


-- Applies a function to the arguments.
-- If the function is an operator from section of the function
-- application operator, we apply it to the arguments using ($ args),
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)



