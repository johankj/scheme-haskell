-- 2. Parsing
-- Exercise 5:
--  Add a Character constructor to LispVal,
--  and create a parser for character literals as described in R5RS.

module Ex5 where

import Parsing hiding (parseChar, parseCharName, parseCharacter)
import Text.ParserCombinators.Parsec

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



