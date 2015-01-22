module Main where

-- ghc --make -o eval Main.hs

import Evaluate
import Parse
import LispError

import System.Environment
import Control.Monad.Error
import System.IO


main :: IO ()
main = do
    args <- getArgs
    case length args of
      0 -> runRepl
      1 -> evalAndPrint $ args !! 0
      _ -> putStrLn "Program takes 0 or 1 arguments"

-- REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
   result <- prompt
   if pred result
      then return ()
      else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint


