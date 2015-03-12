module Parser where

import Text.Parsec.Char (spaces)
import Text.Parsec.String (Parser)

-- Strips trailing whitespace and parses String
parseString :: Parser a -> Parser a
parseString p =
    spaces >>
    p

