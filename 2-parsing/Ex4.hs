-- 2. Parsing
-- Exercise 4:
--  Make parseNumber accept different bases
--  as per Scheme standard.

module Ex4 where

import Data.Char (digitToInt)
import Control.Monad
import Numeric
import Parsing hiding (parseNumber, parseDecimal, parseNumberWithRadixPrefix)
import Text.ParserCombinators.Parsec

-- Original parseNumber which only accepts decimal numbers
parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read

------------

-- parseNumber with support for binary, octal, decimal and hexadecimal
parseNumber' :: Parser LispVal
parseNumber' = parseDecimal <|> parseNumberWithRadixPrefix

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= return . Number . read

-- The following parsers ignores anything after a successful match.
-- E.g.: #x10yz -> (Number 16)
parseNumberWithRadixPrefix :: Parser LispVal
parseNumberWithRadixPrefix = do
    bs  <- char '#' >> oneOf "bodx"
    case bs of
        'b' -> parseBin
        'o' -> parseOctal
        'd' -> parseDecimal
        'x' -> parseHex
        where
          parseBin :: Parser LispVal
          parseBin = do
            num <-  many1 (oneOf "01")
            let readBin = readInt 2 (`elem` "01") digitToInt
            return $ (Number . fst . head . readBin) num
          parseOctal :: Parser LispVal
          parseOctal = do
            num <-  many1 octDigit
            return $ (Number . fst . head . readOct) num
          parseHex :: Parser LispVal
          parseHex = do
            num <-  many1 hexDigit
            return $ (Number . fst . head . readHex) num


--parseNumber' :: Parser LispVal
--parseNumber' = do
--  prefix <- string "#b" <|> string "#o" <|> string "#d" <|> string "#x" <|> string ""
--  number <- many1 digit
--  return $ Number $ case prefix of
--    "#b" -> readBin number -- "#b01hijklmn" -> Error
--    "#o" -> fst $ head $ readOct number -- "#o7hijklmn" -> 7 :/
--    "#x" -> fst $ head $ readHex number -- likewise :/
--    _    -> read number

-- http://tehgeekmeister.wordpress.com/2008/01/12/better-one-line-binary-readers-in-haskell-thanks-to-commenters-and-redditors/
-- Probably the best way of doing it;
-- reinventing what's available in the standard libraries isn't a good thing.
--readBin s = x
--  where
--    [(x,"")] = Numeric.readInt 2 (`elem` "01") digitToInt s

