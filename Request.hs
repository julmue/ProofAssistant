{-# LANGUAGE ExistentialQuantification #-}

module Request

where

import qualified Prelude as Pl

import Parsable
import qualified Formula as F

-- which typeclass could be best used as constrained?
-- Parseable or Evaluatable or a combination of both?

data Request = Request {
    getReqSemantics        :: [String],
    getReqFormula          :: forall a . Parsable a => F.Formula a,
    getReqQueries          :: [PQuery],
    getReqNormalforms      :: [Normalform],
    getReqHelp             :: Bool
}

data Task = Task {
    getTaskSemantics    :: String,
    getTaskFormula      :: forall a . Parsable a => F.Formula a,
    getTaskAction       :: Action
}

data Action = PQuery | Normalform deriving Show

data PQuery = Classify | Valid | Sat | Unsat | Model | Models deriving Show

data Normalform = CND | DNF deriving Show   -- others to follow


