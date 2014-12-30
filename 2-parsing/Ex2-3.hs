-- 2. Parsing
-- Exercise 2-3:
--  R5RS compliant string support
--  by accepting escaped string literals

module Ex2 where

import Control.Monad
import Parsing hiding (parseString, nonEscapeChars, escapeChars)
import Text.ParserCombinators.Parsec

-- Original parseString without R5RS compliancy
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x


-- parseString with support for escaped chars
parseString' :: Parser LispVal
parseString' = do
               char '"'
               x <- many (nonEscapeChars) <|> many (escapeChars)
               char '"'
               return $ String x

-- A parser for non-escaped characters
nonEscapeChars :: Parser Char
nonEscapeChars = noneOf "\n\r\t\\\""

-- A parser for escape characters
-- An escape character is one of: \n, \r, \t, \\, or \"
escapeChars :: Parser Char
escapeChars = oneOf "\n\r\t\\\""

