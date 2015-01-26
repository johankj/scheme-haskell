{-# LANGUAGE ExistentialQuantification #-}

module Primitives where

import LispVal
import LispError

import Control.Monad.Error (throwError, catchError)
import Control.Monad (liftM)

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsLispError a)

-- Mapping of primitive functions in Scheme
primitives :: [(String, [LispVal] -> ThrowsLispError LispVal)]
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
              ("string>=?", stringComparator (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

type Operand = (Integer -> Integer -> Integer)

-- Applies the given operand to the unpacked list of LispVal's
numericOperator :: Operand -> [LispVal] -> ThrowsLispError LispVal
numericOperator op []            = throwError $ NumArgs 2 []
numericOperator op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericOperator op params        = mapM unpackNumber params >>= return . Number . foldl1 op

predicateOperator :: (b -> Bool) -> [b] -> ThrowsLispError LispVal
predicateOperator op = return . Bool . op . head

comparator :: (LispVal -> ThrowsLispError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsLispError LispVal
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
unpackNumber :: LispVal -> ThrowsLispError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return . fst . head $ parsed

unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum = throwError $ TypeMismatch "number" notNum
-- ... string:
unpackString :: LispVal -> ThrowsLispError String
unpackString (String s) = return s
unpackString (Number s) = return $ show s
unpackString (Bool s)   = return $ show s
unpackString notAString = throwError $ TypeMismatch "string" notAString
-- ... boolean:
unpackBoolean :: LispVal -> ThrowsLispError Bool
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


-- List Primitives

-- car examples:
-- (car '(a b c)) = a
-- (car '(a)) = a
-- (car '(a b . c)) = a
-- (car 'a) = error – not a list
-- (car 'a 'b) = error – car only takes one argument

car :: [LispVal] -> ThrowsLispError LispVal
car [List (x : xs)]          = return x
car [DottedList (x : xs) _ ] = return x
car [badArg]                 = throwError $ TypeMismatch "pair" badArg
car badArgList               = throwError $ NumArgs 1 badArgList

-- cdr examples:
-- (cdr '(a b c)) = (b c)
-- (cdr '(a b)) = (b)
-- (cdr '(a)) = NIL
-- (cdr '(a . b)) = b
-- (cdr '(a b . c)) = (b . c)
-- (cdr 'a) = error – not a list
-- (cdr 'a 'b) = error – too many arguments

cdr :: [LispVal] -> ThrowsLispError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

-- cons
cons :: [LispVal] -> ThrowsLispError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


-- Equivalence predicates
eqv :: [LispVal] -> ThrowsLispError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = eqvList eqv [(List arg1), (List arg2)]
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

-- Recursively check equivalence of lists by function-argument
eqvList :: ([LispVal] -> ThrowsLispError LispVal) -> [LispVal] -> ThrowsLispError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] =
    return $ Bool $ (length arg1 == length arg2) &&
                    (all eqvPair $ zip arg1 arg2)
      where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val

equal :: [LispVal] -> ThrowsLispError LispVal
equal [(List arg1), (List arg2)] = eqvList equal [(List arg1), (List arg2)]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNumber, AnyUnpacker unpackString, AnyUnpacker unpackBoolean]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- Equivalence Unpacker
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsLispError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
    `catchError` (const $ return False)



