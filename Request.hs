module Request

where

-- import Prelude (String,Bool,Show,($),fmap,Eq,(++),Bool)

import Data.List (nub)

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
    getReqSemantics        :: [Semantics],
    getReqFormula          :: String,
    getReqQueries          :: [PQuery],
    getReqNormalforms      :: [Normalform],
    getReqHelp             :: Bool
} deriving Show

data Task = Task {
    getTaskSemantics    :: String,
    getTaskFormula      :: String,
    getTaskAction       :: Action
}

data Action = PQuery | Normalform deriving (Show,Eq)

data PQuery = Classify | Valid | Sat | Unsat | Model | Models deriving (Show,Eq)

data Normalform = CNF | DNF deriving (Show,Eq)

data Help = Help deriving (Show,Eq)

data Semantics = PC | L3 deriving (Show,Eq)

data Arg
    = S [Semantics]
    | P [PQuery]
    | F String
    | N [Normalform]
    | H Help
    deriving (Show,Eq)

nubArgs :: Arg -> Arg
nubArgs arg = case arg of
    S s -> S $ nub s
    P q -> P $ nub q
    N n -> N $ nub n
    a   -> a

-- not very elegant ...
-- there has to be a better solution
getSemantics :: [Arg] -> [Semantics]
getSemantics args = case args of
    ((S s):as)  -> s ++ getSemantics as
    (_:as)      -> getSemantics as
    []          -> []

getQueries :: [Arg] -> [PQuery]
getQueries args = case args of
    ((P q):as)  -> q ++ getQueries as
    (_:as)      -> getQueries as
    []          -> []

-- this is not so slick either ...
-- there should be only one formula ...
getFormula :: [Arg] -> String
getFormula args = case args of
    ((F f):_)   -> f
    (_:as)      -> getFormula as
    []          -> []

getNormalforms :: [Arg] -> [Normalform]
getNormalforms args = case args of
    ((N n):as)  -> n ++ getNormalforms as
    (_:as)      -> getNormalforms as
    []          -> []

getHelp :: [Arg] -> Bool
getHelp args = case args of
    ((H Help):_)    -> True
    (_:as)          -> getHelp as
    []              -> False

-- requestConstructor :: [Arg] -> Request
requestConstructor args = Request
    {   getReqSemantics     = nub $ getSemantics args
    ,   getReqFormula       = getFormula args
    ,   getReqQueries       = nub $ getQueries args
    ,   getReqNormalforms   = nub $ getNormalforms args
    ,   getReqHelp          = getHelp args
    }
