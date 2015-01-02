-- 2. Parsing
-- Exercise 6:
--  Add a Float constructor to LispVal,
--  and support R5RS syntax for decimals.

module Ex6 where

import Parsing hiding (parseFloat)
import Text.ParserCombinators.Parsec

-- parse float as per R5RS
parseFloat :: Parser LispVal
parseFloat = do
    decimalPart <- many1 digit
    char '.'
    fractionPart <- many1 digit
    return $ Float . read $ decimalPart ++ "." ++ fractionPart

