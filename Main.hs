import Semantics
import Prop
import PropSemanticsPC
import ParserProp
import Request
import qualified ParserRequestArguments as Args
import qualified Formula as F
import FormulaPCLaws
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


{- ToDo:
    Languages of Formulas are not properly linked to the semantics ...
    as of now it is possible to attempt an evaluation of a LPC formula
    under PC semantics -- which throws.

    It should be possible to derive the set of languages a formula belongs to
    e.g.:   > derive "a && b": pc, lpc, l3, s4, ...
            > derive "Forall.a: Px": lpc, ...
    and return sensible error messages according to this classification.
-}
