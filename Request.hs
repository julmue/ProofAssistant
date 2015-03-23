module Request
    ( Request(..)
    , Task(..)
    , Action(..)
    , SemanticsReq(..)
    , PropertyReq(..)
    , NormalFormReq(..)
    )
where

-- import Prelude (String,Bool,Show,($),fmap,Eq,(++),Bool)

import Control.Applicative
import Data.List (nub,groupBy)

-- import Formula
-- import Semantics

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
    getReqSemantics         :: [SemanticsReq],
    getReqClassify          :: Bool,
    getReqProperties        :: [PropertyReq],
    getReqModels            :: Bool,
    getReqNormalForms       :: [NormalFormReq],
    getReqHelp              :: Bool
} deriving (Show,Eq)

data Task = Task {
    getTaskFormula          :: String,
    getTaskSemantics        :: SemanticsReq,
    getTaskAction           :: Action
} deriving (Show,Eq)

data Action
    = ClassifyAction
    | PropertyAction PropertyReq
    | ModelAction
    | NFAction  NormalFormReq
    | HelpAction
    deriving (Show,Eq)

-- the different form of queries are classified by their answer types:
-- classify :: FProp = Valid, Sat, Unsat
-- sat, unsat, valid :: Bool
-- model, models :: [Model]
-- normalform :: Formula

data SemanticsReq
    = PCReq
    | K3Req
    | L3Req
    | LPReq
    | RMReq
    deriving (Show,Eq)

data PropertyReq
    = ValidReq
    | SatReq
    | UnsatReq
    deriving (Show,Eq)

data NormalFormReq
    = CNFReq
    | DNFReq
    deriving (Show,Eq)


