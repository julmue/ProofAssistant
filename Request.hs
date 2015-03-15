{-# LANGUAGE ExistentialQuantification #-}

module Request

where

import Prelude (String,Bool,Show)

import qualified Formula as F

-- which typeclass could be best used as constrained?
-- Parseable or Evaluatable or a combination of both?

data Request = Request {
    getReqSemantics        :: [String],
    getReqFormula          :: F.Formula DummyBox,
    getReqQueries          :: [PQuery],
    getReqNormalforms      :: [Normalform],
    getReqHelp             :: Bool
}

data Task = Task {
    getTaskSemantics    :: String,
    getTaskFormula      :: F.Formula DummyBox,
    getTaskAction       :: Action
}

data Action = PQuery | Normalform deriving Show

data PQuery = Classify | Valid | Sat | Unsat | Model | Models deriving Show

data Normalform = CND | DNF deriving Show   -- others to follow

class Dummy a where
    dummy :: a

data DummyBox = forall a. Dummy a => DB a
