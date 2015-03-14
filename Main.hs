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
    what:
    -- default: classify
    -- classify:    tautology, neutrality, contradiction
    -- valid:       validity check
    -- sat:         satisfiability check
    -- unsat:       unsatisfiability check
    -- model:       one model
    -- models:      all models
    -- nf:          normal form (CNF,DNF,...)
                    * default: CNF
    -- help


    probably best to put that all in a record;
    some kind of "request objects" ... then a request parse could be implemened

-}

-- data Request = Request {
--     getSemantics    :: Semantic
--     getFormula      :: F.Formula
--     getActions      :: [Action]
-- }

-- Semantic :: truth values and evaluation function

data Semantic (Formula a)
    deriving Show

main :: IO ()
main = undefined



-- helper functions
parseExp :: Parser a -> String -> a
parseExp p s = case parse p "" s of
    (Right a)     -> a
    (Left err)    -> error (show err)


parseProp = parseExp formulaProp

