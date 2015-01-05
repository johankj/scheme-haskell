module Evaluate where

-- ghci -i2-parsing 3-evaluation/Evaluate.hs
-- or
-- ghc -main-is Evaluate -i2-parsing --make -o eval 3-evaluation/Evaluate.hs
-- ./eval "(+ 1 2 3)"

import Parsing
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head


-- evaluate
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

-- Applies a function to the arguments.
-- If the function is an operator from section of the function
-- application operator, we apply it to the arguments using ($ args),
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

-- Mapping of primitive functions in Scheme
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericOperator (+)),
              ("-", numericOperator (-)),
              ("*", numericOperator (*)),
              ("/", numericOperator div),
              ("mod", numericOperator mod),
              ("quotient", numericOperator quot),
              ("remainder", numericOperator rem)]

type Operand = (Integer -> Integer -> Integer)

-- Applies the given operand to the unpacked list of LispVal's
numericOperator :: Operand -> [LispVal] -> LispVal
numericOperator op params = Number $ foldl1 op $ map unpackNumber params

-- Unpacks a LispVal
unpackNumber :: LispVal -> Integer
unpackNumber (Number n) = n
unpackNumber (String n) = let parsed = reads n :: [(Integer, String)] in
                           if null parsed
                              then 0
                              else fst . head $ parsed

unpackNumber (List [n]) = unpackNumber n
unpackNumber _ = 0



