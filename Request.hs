module Request

where

import Prelude (String,Bool,Show)

import qualified Formula as F

{- clank
    command line arguments:
    + semantics ('s'):
        + pc
        + l3
        * (default: pc)
    + formula ('f'):
        + <String>
        * default ""
    + property queries ('p'):
        * default: classify
        + classify:    tautology, neutrality, contradiction
        + valid:       validity check
        + sat:         satisfiability check
        + unsat:       unsatisfiability check
        + model:       one model
        + models:      all models
    + normal forms ('n'):
        + cnf,
        + dn
        * default: CNF
    + help ('h')

    probably best to put that all in a record;
    some kind of "request objects" ... then a request parser could be implemened

-}


-- which typeclass could be best used as constrained?
-- Parseable or Evaluatable or a combination of both?

data Request = Request {
    getReqSemantics        :: [String],
    getReqFormula          :: String,
    getReqQueries          :: [PQuery],
    getReqNormalforms      :: [Normalform],
    getReqHelp             :: Bool
}

data Task = Task {
    getTaskSemantics    :: String,
    getTaskFormula      :: String,
    getTaskAction       :: Action
}

data Action = PQuery | Normalform deriving Show

data PQuery = Classify | Valid | Sat | Unsat | Model | Models deriving Show

data Normalform = CNF | DNF deriving Show

data Help = Help deriving Show

data Semantics = PC | L3 deriving Show

data Arg
    = S [Semantics]
    | P [PQuery]
    | F String
    | NF [Normalform]
    | H Help
    deriving Show
