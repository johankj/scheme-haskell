{-# LANGUAGE ExistentialQuantification #-}

module Evaluate where

-- ghci -i2-parsing evaluation/Evaluate.hs
-- or
-- ghc -main-is Evaluate -i2-parsing --make -o eval evaluation/Evaluate.hs
-- ./eval "(+ 1 2 3)"

import Parsing
import System.Environment
import Control.Monad.Error

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ (readExpr (head args) >>= eval)
    putStrLn $ extractValue $ trapError evaled

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

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
eval form@(List (Atom "cond" : [])) = throwError $ BadSpecialForm "no true clause in cond expression: " form
eval form@(List (Atom "cond" : clauses)) =
    case head clauses of
      List [Atom "else", expr] -> eval expr
      -- Piggyback on if-implementation
      List [test, expr]        -> eval $ List [Atom "if",
          test,
          expr,
          List (Atom "cond" : tail clauses)]
      otherwise -> throwError $ BadSpecialForm "ill-formed cond expression: " form

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
              ("string>=?", stringComparator (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

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


-- List Primitives

-- car examples:
-- (car '(a b c)) = a
-- (car '(a)) = a
-- (car '(a b . c)) = a
-- (car 'a) = error – not a list
-- (car 'a 'b) = error – car only takes one argument

car :: [LispVal] -> ThrowsError LispVal
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

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

-- cons
cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList


-- Equivalence predicates
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]         = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = eqvList eqv [(List arg1), (List arg2)]
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

-- Recursively check equivalence of lists by function-argument
eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] =
    return $ Bool $ (length arg1 == length arg2) &&
                    (all eqvPair $ zip arg1 arg2)
      where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                    Left err -> False
                                    Right (Bool val) -> val

equal :: [LispVal] -> ThrowsError LispVal
equal [(List arg1), (List arg2)] = eqvList equal [(List arg1), (List arg2)]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                       [AnyUnpacker unpackNumber, AnyUnpacker unpackString, AnyUnpacker unpackBoolean]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- Equivalence Unpacker
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do unpacked1 <- unpacker arg1
       unpacked2 <- unpacker arg2
       return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

