-- 2. Parsing
-- Exercise 1:
--  Rewrite parseNumber, without liftM, using
--    1. do-notation
--    2. explicit sequencing with the >>= operator

module Ex1 where

import Control.Monad
import Parsing hiding (parseNumber)
import Text.ParserCombinators.Parsec

-- Original, using liftM
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- Using do-notation
parseNumber' :: Parser LispVal
parseNumber' = do
                x <- many1 digit
                return $ (Number . read) x

-- Using >>=
parseNumber'' :: Parser LispVal
parseNumber'' = many1 digit >>= return . Number . read

