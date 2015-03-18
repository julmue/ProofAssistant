import qualified Formula as F
import FormulaPCLaws
import Parser
import ParserFormula
import ParserProp
import qualified ParserRequest as PR
import PrettyPrint
import Prop
import PropSemanticsPC
import qualified Request as REQ
import Semantics

import Text.Parsec
import Text.Parsec.String


main :: IO ()
main = undefined

data Prop
    = Valid
    | Sat
    | Unsat
    deriving (Show, Eq)

-- processTask :: Task -> IO ()
-- processTask t =
--     let f = getTaskFormula t
--         s = getTaskSemantics t
--     in  case getTaskaction t of
--         (PQAction pqa)  -> processPQAction f s pqa
--         (NfAction nfa)  -> processNfAction f s nfa
--         (HAction)       -> getHelp
--
-- processPQAction :: String -> Semantics ->
-- processPQAction f s pqa


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
