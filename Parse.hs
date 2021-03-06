module Parse where

import LispVal
import LispError

import Data.Char
import Data.Ratio (Rational (..), (%))
import Data.Complex (Complex (..))
import Control.Monad
import Control.Monad.Error (throwError)
import System.Environment
import Numeric (readInt, readHex, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- A parser for whitespace
-- Skips 1 or more spaces
spaces :: Parser ()
spaces = skipMany1 space

-- A parser for Scheme strings
parseString :: Parser LispVal
parseString = do
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

-- A parser for symbols in Scheme
symbol :: Parser Char
symbol = oneOf "+-*/$!%&:^|<>=?@_~"

-- A parser for atoms
-- Atoms are: <letter>|<symbol> (<letter>|<symbol>|<digit>)*
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ Atom atom

-- A Scheme list is either a normal list or a dotted list
parseList :: Parser LispVal
parseList = do
    char '('
    x <- parseNormalList <|> parseDottedList
    char ')'
    return x

-- Scheme lists are expressions seperated by spaces
parseNormalList :: Parser LispVal
parseNormalList = try $ liftM List $ parseExpr `sepBy` spaces

-- Improper/dotted lists are lists which doesn't end with the empty list.
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- parseExpr `endBy` spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseBool :: Parser LispVal
parseBool = try $ do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

-- Parse ratios, e.g. 5/4
parseRatio :: Parser LispVal
parseRatio = try $ do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio ((read x) % (read y))

-- Parse complex numbers, e.g. 3+2i
parseComplex :: Parser LispVal
parseComplex = try $ do
    a <- fmap toDouble (parseFloat <|> parseDecimal)
    char '+'
    b <- fmap toDouble (parseFloat <|> parseDecimal)
    char 'i'
    return $ Complex (a :+ b)
    where toDouble (Float x) = x
          toDouble (Number x) = fromIntegral x

-- A parser for Scheme numbers
-- parseNumber with support for binary, octal, decimal and hexadecimal
parseNumber :: Parser LispVal
parseNumber = parseDecimal <|> parseNumberWithRadixPrefix

parseDecimal :: Parser LispVal
parseDecimal = many1 digit >>= return . Number . read

-- The following parsers ignores anything after a successful match.
-- E.g.: #x10yz -> (Number 16)
parseNumberWithRadixPrefix :: Parser LispVal
parseNumberWithRadixPrefix = do
    bs <- try $ char '#' >> oneOf "bodx"
    case bs of
      'b' -> parseBin
      'o' -> parseOctal
      'd' -> parseDecimal
      'x' -> parseHex
      where
        parseBin :: Parser LispVal
        parseBin = do
          num <- many1 (oneOf "01")
          let readBin = readInt 2 (`elem` "01") digitToInt
          return $ (Number . fst . head . readBin) num
        parseOctal :: Parser LispVal
        parseOctal = do
          num <- many1 octDigit
          return $ (Number . fst . head . readOct) num
        parseHex :: Parser LispVal
        parseHex = do
          num <- many1 hexDigit
          return $ (Number . fst . head . readHex) num

-- parse float as per R5RS
parseFloat :: Parser LispVal
parseFloat = try $ do
    decimal <- many1 digit
    char '.'
    fraction <- many1 digit
    return $ Float . read $ decimal ++ "." ++ fraction

-- parse a char or a char-name
parseChar :: Parser LispVal
parseChar = parseCharName <|> parseCharacter

-- parse a char-name (space/newline)
parseCharName :: Parser LispVal
parseCharName = try $ do
    string "#\\"
    x <- string "space" <|> string "newline"
    return $ case x of
               "space"  -> Character ' '
               "newline"-> Character '\n'

-- parse a character (letter/digit/symbol)
parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    (letter <|> digit <|> symbol) >>= return . Character

-- Parse quoted expressions
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Parse quasi-quotations
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

-- Parse unquotes
parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

-- Very basic Vector parser
-- This currently doesn't honor the R5RS:
--  "occupies less space than a list of the same length, and the average time
--  required to access a randomly chosen element is typically less for the
--  vector than for the list."
parseVector :: Parser LispVal
parseVector = do
    try $ string "#("
    x <- parseExpr `sepBy` spaces
    char ')'
    return $ Vector (length x, x)

-- A parser for Scheme Expressions
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseComplex
         <|> parseFloat
         <|> parseRatio
         <|> parseNumber
         <|> parseBool
         <|> parseChar
         <|> parseList
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> parseVector
