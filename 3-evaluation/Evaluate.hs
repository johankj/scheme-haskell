module Evaluate where

-- ghci -i2-parsing 3-evaluation/Evaluate.hs
-- or
-- ghc -main-is Evaluate -i2-parsing --make -o eval 3-evaluation/Evaluate.hs
-- ./eval "(+ 1 2 3)"

import Parsing
import System.Environment
import Control.Monad.Error

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ (readExpr (head args) >>= eval)
    putStrLn $ extractValue $ trapError evaled


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

eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Applies a function to the arguments.
-- If the function is an operator from section of the function
-- application operator, we apply it to the arguments using ($ args),
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

-- Mapping of primitive functions in Scheme
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericOperator (+)),
              ("-", numericOperator (-)),
              ("*", numericOperator (*)),
              ("/", numericOperator div),
              ("mod", numericOperator mod),
              ("quotient", numericOperator quot),
              ("remainder", numericOperator rem),
              ("symbol?", predicateOperator symbolp),
              ("string?", predicateOperator stringp),
              ("number?", predicateOperator numberp),
              ("bool?", predicateOperator boolp),
              ("list?", predicateOperator listp),
              ("=", numberComparator (==)),
              ("<", numberComparator (<)),
              (">", numberComparator (>)),
              ("/=", numberComparator (/=)),
              (">=", numberComparator (>=)),
              ("<=", numberComparator (<=)),
              ("&&", booleanComparator (&&)),
              ("||", booleanComparator (||)),
              ("string=?", stringComparator (==)),
              ("string<?", stringComparator (<)),
              ("string>?", stringComparator (>)),
              ("string<=?", stringComparator (<=)),
              ("string>=?", stringComparator (>=))]

type Operand = (Integer -> Integer -> Integer)

-- Applies the given operand to the unpacked list of LispVal's
numericOperator :: Operand -> [LispVal] -> ThrowsError LispVal
numericOperator op []            = throwError $ NumArgs 2 []
numericOperator op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericOperator op params        = mapM unpackNumber params >>= return . Number . foldl1 op

predicateOperator :: (b -> Bool) -> [b] -> ThrowsError LispVal
predicateOperator op = return . Bool . op . head

comparator :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
comparator unpacker op args =
    if length args /= 2
    then throwError $ NumArgs 2 args
    else do left <- unpacker $ args !! 0
            right <- unpacker $ args !! 1
            return $ Bool $ left `op` right

stringComparator = comparator unpackString
numberComparator = comparator unpackNumber
booleanComparator = comparator unpackBoolean

-- Unpack LispVal as ...
-- ... number:
unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return . fst . head $ parsed

unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum = throwError $ TypeMismatch "number" notNum
-- ... string:
unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString (Number s) = return $ show s
unpackString (Bool s)   = return $ show s
unpackString notAString = throwError $ TypeMismatch "string" notAString
-- ... boolean:
unpackBoolean :: LispVal -> ThrowsError Bool
unpackBoolean (Bool b) = return b
unpackBoolean notABool = throwError $ TypeMismatch "boolean" notABool

-- Predicates
symbolp :: LispVal -> Bool
symbolp (Atom _) = True
symbolp _        = False

stringp :: LispVal -> Bool
stringp (String _) = True
stringp _          = False

numberp :: LispVal -> Bool
numberp (Number _) = True
numberp (Float _)  = True
numberp _          = False

boolp :: LispVal -> Bool
boolp (Bool _) = True
boolp _ = False

listp :: LispVal -> Bool
listp (List _) = True
listp (DottedList _ _) = True
listp _ = False


