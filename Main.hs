import Prop
import PropSemantics
import ParserProp
import qualified Formula as F
import ParserFormula
import Parser

import PrettyPrint

import Text.Parsec
import Text.Parsec.String

main :: IO ()
main = undefined



-- helper functions
parseExp :: Parser a -> String -> a
parseExp p s = case parse p "" s of
    (Right a)     -> a
    (Left err)    -> error (show err)


parseProp = parseExp formulaProp

