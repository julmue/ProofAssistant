module Request

where

-- import Prelude (String,Bool,Show,($),fmap,Eq,(++),Bool)

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


{- flags -}

type Flags = [String]

flagFormula = ["-f","--formula"]
flagSemantics = ["-s","--semantics"]
flagClassification = ["-c","--classify"]
flagProperty = ["-p","--properties"]
flagModell = ["-m","-ms","--models","--model"]
flagNormalForm = ["-n","--normalform"]
flagHelp = ["-h","--help"]

flags = flagFormula ++ flagSemantics ++ flagClassification ++ flagProperty ++
        flagModell ++ flagNormalForm ++ flagHelp

data Arg = Flag String | Option String deriving (Show,Eq)

classifyArgs :: Flags -> String -> Arg
classifyArgs fl = \s -> if s `elem` fl
                        then Flag s
                        else Option s

-- groupArgs // there should be a better name
groupArgs :: [String] -> [[Arg]]
groupArgs = (groupBy (==) . fmap (classifyArgs flags))










-- not very elegant ...

-- requestConstructor :: [Arg] -> Request
-- requestConstructor args = Request
--     {   getReqFormula       = getFormula args
--     ,   getReqClassify      = getClassify args
--     ,   getReqSemantics     = nub $ getSemantics args
--     ,   getReqProps         = nub $ getProps args
--     ,   getReqModels        = nub $ getModels args
--     ,   getReqNormalForms   = nub $ getNormalForms args
--     ,   getReqHelp          = getHelp args
--     }
--
-- tasksConstructor :: Request -> [Task]
-- tasksConstructor req =
--     let f = getReqFormula req
--     in  [ Task f s ClassifyAction   | s <- (getReqSemantics req), getReqClassify req] ++
--         [ Task f s (PropAction p)   | s <- (getReqSemantics req), p <- (getReqProps req) ] ++
--         [ Task f s (ModelAction m)  | s <- (getReqSemantics req), m <- (getReqModels req) ] ++
--         [ Task f s (NFAction nf)    | s <- (getReqSemantics req), nf <- (getReqNormalForms req) ] ++
--         [ Task [] PC HelpAction | (getReqHelp req) ]
--
-- toTasks :: [Arg] -> [Task]
-- toTasks = tasksConstructor . requestConstructor
