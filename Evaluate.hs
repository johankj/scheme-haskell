module Evaluate where

import LispVal
import LispError
import Primitives
import IOPrimitives
import Environment

import System.Environment
import Control.Monad.Error
import System.IO

-- evaluate
eval :: LispEnv -> LispVal -> IOThrowsLispError LispVal
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
-- Set variables
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
-- Define variables
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
-- Define functions
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarArgs varargs env [] body
-- Implement load to extend the environment
eval env (List [Atom "load", String filename]) = 
     load filename >>= liftM last . mapM (eval env)
-- Evaluate functions
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Evaluate cond
evalCondClause :: LispEnv -> LispVal -> LispVal -> IOThrowsLispError LispVal -> IOThrowsLispError LispVal
evalCondClause env pred conseq alt = do
    result <- eval env pred
    case result of
      Bool True  -> eval env conseq
      Bool False -> alt
      otherwise -> throwError $ TypeMismatch "boolean" result

evalCond :: LispEnv -> [LispVal] -> IOThrowsLispError LispVal
evalCond env [List [Atom "else", conseq]] = eval env conseq
evalCond env [List [pred, conseq]] = evalCondClause env pred conseq (return $ List [])
evalCond env (List [pred, conseq] : xs) = evalCondClause env pred conseq (evalCond env xs)
evalCond env [] = throwError $ BadSpecialForm "no true clause in cond expression" (List [Atom "cond"])
evalCond env badArgList = throwError $ BadSpecialForm "illegal clause in cond expression" $ head badArgList

-- Evaluate case
evalCaseClause :: LispEnv -> LispVal -> LispVal -> LispVal -> IOThrowsLispError LispVal -> IOThrowsLispError LispVal
evalCaseClause env key val conseq alt = do
    result <- liftThrows $ eqv [key, val]
    case result of
      Bool True  -> eval env conseq
      Bool False -> alt
      otherwise  -> throwError $ TypeMismatch "boolean" result

evalCase :: LispEnv -> LispVal -> [LispVal] -> IOThrowsLispError LispVal
evalCase env _ [List [Atom "else", conseq]] = eval env conseq
evalCase env key [List [val, conseq]] = evalCaseClause env key val conseq (return $ List [])
evalCase env key (List [val, conseq] : xs) = evalCaseClause env key val conseq (evalCase env key xs)
evalCase env _ badArgList = throwError $ BadSpecialForm "invalid cond clause" (badArgList !! 0)

-- Make functions
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

-- Applies a function to the arguments.
-- If the function is an operator/PrimitiveFunc from section of the function
-- application operator, we apply it to the arguments using ($ args),
-- If it's a new Func, we bind the args to an environment and run the body
-- in that environment.
apply :: LispVal -> [LispVal] -> IOThrowsLispError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params vararg body closure) args =
    if num params /= num args && vararg == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs vararg >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
            Nothing -> return env

primitiveBindings :: IO LispEnv
primitiveBindings = nullEnv >>= (flip bindVars $ allFuncs)
    where
      allFuncs = primFuncs ++ ioFuncs
      primFuncs = map (makeFunc PrimitiveFunc) primitives
      ioFuncs = map (makeFunc IOFunc) ioPrimitives
      makeFunc constructor (var, func) = (var, constructor func)


