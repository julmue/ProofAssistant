import Semantics
import Prop
import PropSemanticsPC
import ParserProp
import qualified Formula as F
import FormulaPCLaws
import ParserFormula
import Parser

import PrettyPrint

import Text.Parsec
import Text.Parsec.String

{- clank
    command line arguments:
    -- semantics: PC, L3, ...
       -- default: PC
    -- formula: "<String>"
    property queries:
    -- default: classify
    -- classify:    tautology, neutrality, contradiction
    -- valid:       validity check
    -- sat:         satisfiability check
    -- unsat:       unsatisfiability check
    -- model:       one model
    -- models:      all models
    transformations:
    -- nf:          normal form (CNF,DNF,...)
                    * default: CNF
    -- help

    probably best to put that all in a record;
    some kind of "request objects" ... then a request parse could be implemened

-}

data Request a b = Request {
    getReqSemantics        :: [String],
    getReqFormula          :: F.Formula a, -- is it possible to parse a formula to more than one type? 
    getReqQueries          :: [PQuery],
    getReqNormalforms      :: [Normalform],
    getReqHelp             :: Bool
}

data Task a b = Task {
    getTaskSemantics    :: Semantics a b,
    getTaskFormula      :: F.Formula a,
    getTaskAction       :: Action
}

data Action = PQuery | Normalform deriving Show

data PQuery = Classify | Valid | Sat | Unsat | Model | Models deriving Show

data Normalform = CND | DNF deriving Show   -- others to follow


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
