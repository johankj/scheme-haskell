module Main where

-- ghc --make -o eval Main.hs

import Evaluate
import LispVal
import LispError
import Parse
import IOHelpers

import System.Environment
import Control.Monad.Error
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case length args of
      0 -> runRepl
      1 -> runOne $ args !! 0
      _ -> putStrLn "Program takes 0 or 1 arguments"

-- REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: LispEnv -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: LispEnv -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint


