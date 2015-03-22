module Request

where

-- import Prelude (String,Bool,Show,($),fmap,Eq,(++),Bool)

import Control.Applicative
import Data.List (nub,groupBy)

import qualified Formula as F

{- clank
    command line arguments:
    + semantics ('-s'):
        + pc
        + l3
        * (default: pc)
    + formula ('-f'):
        + <String>
        * default ""
    + classification request ('-c')
        (checks if formula is valid, sat, unsat under given semantics.)
    + property queries ('p'):
        + valid:       validity check
        + sat:         satisfiability check
        + unsat:       unsatisfiability check
    + model queries ('-m'):
        + model:       one model (if any)
        + models:      all models (if any)
    + normal forms ('-n'):
        + cnf,
        + dnf,
        * default: CNF
    + help ('-h')
-}

data Request = Request {
    getReqFormulas          :: [String],
    getReqSemantics         :: [Semantics],
    getReqClassify          :: Bool,
    getReqProps             :: [Prop],
    getReqModels            :: Bool,
    getReqNormalForms       :: [NormalForm],
    getReqHelp              :: Bool
} deriving (Show, Eq)

data Task = Task {
    getTaskFormula      :: String,
    getTaskSemantics    :: Semantics,
    getTaskAction       :: Action
} deriving (Show, Eq)

data Action
    = ClassifyAction
    | PropAction Prop
    | ModelAction
    | NFAction  NormalForm
    | HelpAction
    deriving (Show,Eq)

-- the different form of queries are classified by their answer types:
-- classify :: FProp = Valid, Sat, Unsat
-- sat, unsat, valid :: Bool
-- model, models :: [Model]
-- normalform :: Formula

data Semantics
    = PC
    | L3
    deriving (Show,Eq)

data Prop
    = Valid
    | Sat
    | Unsat
    deriving (Show,Eq)

data NormalForm
    = CNF
    | DNF
    deriving (Show,Eq)

data Help = Help deriving (Show,Eq)
