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
    + classification request ('c')
        (checks if formula is valid, sat, unsat under given semantics.)
    + property queries ('p'):
        + valid:       validity check
        + sat:         satisfiability check
        + unsat:       unsatisfiability check
    + model queries ('m'):
        + model:       one model (if any)
        + models:      all models (if any)
    + normal forms ('n'):
        + cnf,
        + dnf,
        * default: CNF
    + help ('h')
-}

data Request = Request {
    getReqFormula           :: String,
    getReqSemantics         :: [Semantics],
    getReqClassify          :: Bool,
    getReqProps             :: [Prop],
    getReqModels            :: [Model],
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
    | ModelAction Model
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

data Classify = Classify deriving (Show,Eq)

data Prop
    = Valid
    | Sat
    | Unsat
    deriving (Show,Eq)

data Model
    = Model
    | Models
    deriving (Show,Eq)

data NormalForm
    = CNF
    | DNF
    deriving (Show,Eq)

data Help = Help deriving (Show,Eq)

data Arg
    = ArgFormula String
    | ArgSemantics [Semantics]
    | ArgProps [Prop]
    | ArgClassify -- classification request
    | ArgModel Model
    | ArgNFs [NormalForm]
    | ArgHelp -- help request
    deriving (Show,Eq)

-- this is not so slick either ...
-- there should be only one formula ...
getFormula :: [Arg] -> String
getFormula args = case args of
    ((ArgFormula f):_)   -> f
    (_:as)                  -> getFormula as
    []                      -> []

-- not very elegant ...
-- there has to be a better solution
getSemantics :: [Arg] -> [Semantics]
getSemantics args = case args of
    ((ArgSemantics s):as)   -> s ++ getSemantics as
    (_:as)                  -> getSemantics as
    []                      -> []

getClassify :: [Arg] -> Bool
getClassify args = case args of
    (ArgClassify :_)    -> True
    (_:as)              -> getClassify as
    []                  -> False

getProps :: [Arg] -> [Prop]
getProps args = case args of
    ((ArgProps p):as)   -> p ++ getProps as
    (_:as)              -> getProps as
    []                  -> []

getModels :: [Arg] -> [Model]
getModels args = case args of
    ((ArgModel Model) :as)  -> [Model] ++ getModels as
    ((ArgModel Models) :as) -> [Models] ++ getModels as
    (_:as)          -> getModels as
    []              -> []

getNormalForms :: [Arg] -> [NormalForm]
getNormalForms args = case args of
    ((ArgNFs n):as)     -> n ++ getNormalForms as
    (_:as)              -> getNormalForms as
    []                  -> []

getHelp :: [Arg] -> Bool
getHelp args = case args of
    (ArgHelp:_)     -> True
    (_:as)          -> getHelp as
    []              -> False

requestConstructor :: [Arg] -> Request
requestConstructor args = Request
    {   getReqFormula       = getFormula args
    ,   getReqClassify      = getClassify args
    ,   getReqSemantics     = nub $ getSemantics args
    ,   getReqProps         = nub $ getProps args
    ,   getReqModels        = nub $ getModels args
    ,   getReqNormalForms   = nub $ getNormalForms args
    ,   getReqHelp          = getHelp args
    }

taskConstructor :: Request -> [Task]
taskConstructor req =
    let f = getReqFormula req
    in  [ Task f s ClassifyAction   | s <- (getReqSemantics req), getReqClassify req] ++
        [ Task f s (PropAction p)   | s <- (getReqSemantics req), p <- (getReqProps req) ] ++
        [ Task f s (ModelAction m)  | s <- (getReqSemantics req), m <- (getReqModels req) ] ++
        [ Task f s (NFAction nf)    | s <- (getReqSemantics req), nf <- (getReqNormalForms req) ] ++
        [ Task [] PC HelpAction | (getReqHelp req) ]

