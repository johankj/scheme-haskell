module IOPrimitives where

import LispVal
import LispError
import IOHelpers

import System.IO (IOMode(..), openFile, hClose, hGetLine, hPrint, stdin, stdout)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM)

-- IO Primitives
applyIO (IOFunc func) args = func args

ioPrimitives :: [(String, [LispVal] -> IOThrowsLispError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsLispError LispVal
applyProc [func, List args] = applyIO func args
applyProc (func : args)     = applyIO func args

-- wrap the Haskell openFile-function
makePort :: IOMode -> [LispVal] -> IOThrowsLispError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- wrap the Haskell hClose-function
closePort :: [LispVal] -> IOThrowsLispError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

-- wrap the Haskell hGetLine-function
readProc :: [LispVal] -> IOThrowsLispError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

-- Write a LispVal to a Port
writeProc :: [LispVal] -> IOThrowsLispError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

-- wrap the Haskell readFile-function
readContents :: [LispVal] -> IOThrowsLispError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

-- load reads and parses a file full of statements
-- but doesn't bind into the local environment.
load :: String -> IOThrowsLispError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsLispError LispVal
readAll [String filename] = liftM List $ load filename

