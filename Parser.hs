module Parser where

-- Strips trailing whitespace and parses String
parseString :: Parser a -> Parser a
parserString p =
    whiteSpace >>
    p

