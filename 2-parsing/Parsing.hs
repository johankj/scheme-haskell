module Parsing where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Char
import System.Environment
import Control.Monad
import Numeric


-- The data in the program
-- It can be any Lisp value
data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             deriving(Show)

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

parseBool :: Parser LispVal
parseBool = try $ do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

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


-- A parser for Scheme Expressions
parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseBool
         <|> parseChar

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
      Left err -> "No match: " ++ show err
      Right val -> "Found value: " ++ show val


main :: IO()
main = do
        args <- getArgs
        putStrLn (readExpr (args !! 0))
