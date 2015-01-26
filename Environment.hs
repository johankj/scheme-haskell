module Environment where

import LispError

import Control.Monad (liftM)
import Control.Monad.Error (ErrorT, throwError, runErrorT)
import Control.Monad.IO.Class (liftIO)
import Data.IORef

type Environment a = IORef [(String, IORef a)]

nullEnv :: IO (Environment a)
nullEnv = newIORef []

isBound :: Environment a -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . lookupBool var
    where lookupBool var = maybe False (const True) . lookup var

-- Get's an already bound variable
getVar :: Environment a -> String -> IOThrowsError a a
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable" var)
          (liftIO . readIORef)
          (lookup var env)

-- Overrides an already bound variable and returns the new value
setVar :: Environment a -> String -> a -> IOThrowsError a a
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable" var)
          (liftIO . (flip writeIORef value))
          (lookup var env)
    return value

-- Creates a new variable or overrides an already bound variable
defineVar :: Environment a -> String -> a -> IOThrowsError a a
defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var value >> return value
       else liftIO $ do
            valueRef <- newIORef value
            env <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Environment a -> [(String, a)] -> IO (Environment a)
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)


