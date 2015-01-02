-- 2. Parsing
-- Exercise 7:
--  Add data types and parsers to support the full numeric
--  tower of Scheme numeric types.
--  Haskell has built-in types to represent many of these;
--  check the Prelude. For the others, you can define compound
--  types that represent eg. a Rational as a numerator and denominator,
--  or a Complex as a real and imaginary part (each itself a Real).

module Ex7 where

import Parsing hiding (parseRatio, parseComplex)
import Data.Complex (Complex (..))
import Data.Ratio (Rational (..), (%))
import Text.ParserCombinators.Parsec

-- Parse ratios, e.g. 5/4
-- Remember to add Ratio to LispVal
parseRatio :: Parser LispVal
parseRatio = try $ do
    num <- many1 digit
    char '/'
    denom <- many1 digit
    return $ Ratio ((read num) % (read denom))

-- Parse complex numbers, e.g. 3+2i
-- Remember to add Complex to LispVal
parseComplex :: Parser LispVal
parseComplex = try $ do
    a <- fmap toDouble (parseFloat <|> parseDecimal)
    char '+'
    b <- fmap toDouble (parseFloat <|> parseDecimal)
    char 'i'
    return $ Complex (a :+ b)
    where toDouble (Float x) = x
          toDouble (Number x) = fromIntegral x

