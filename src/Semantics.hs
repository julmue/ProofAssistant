{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE ExistentialQuantification #-}

module Semantics
    ( Semantics (..)
    , Property(..)
    , assignments
    , makeModels
    , makeShowModels
    )
where

import Prelude hiding (lookup)

import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (groupBy)
import Data.Map (fromList, lookup)
import Data.Maybe (fromMaybe)

import Formula hiding (True, False)
import Misc

data Property
    = Valid
    | Sat
    | Unsat
    deriving (Show, Eq)

-- class of semantics for a
data Semantics a b = Semantics {
    models :: Formula a -> [b],
    valid :: Formula a -> Bool,
    sat :: Formula a -> Bool,
    unsat :: Formula a -> Bool
}

-- interpretation / assignment

-- set of possible assignment functions for a formula
-- could all truth values be grouped into a type class?
assignments :: (Eq a, Ord a, Ord b) =>[b] -> Formula a -> [a -> b]
assignments truthValues formula =
    let atoms = atomsSet formula
        atomTValuePairs = cartProd atoms truthValues
        partitions = groupBy ((==) `on` fst) atomTValuePairs
        assnmtLookupTables = combination partitions
    in makeAssignment <$> assnmtLookupTables
    where makeAssignment :: Ord a => [(a,b)] -> a -> b
          makeAssignment assnmtLookupTable a =
            let m = fromList assnmtLookupTable
            in fromMaybe (error "Error(Assignment): variable not in assignment function")
               (lookup a m)

makeModels :: forall b a.(Ord a, Ord b, Eq b) =>
    [b] -> [b] -> (Formula b -> Formula b) -> Formula a -> [a -> b]
makeModels truthValues distinguishedTVals evalFn formula =
   let assnmts = assignments truthValues formula
       mask = evalFn <$> sequence (onAtoms <$> assignments truthValues formula) formula
    in [ model | (model, tValue) <- zip assnmts mask, tValue `elem`(Atom <$> distinguishedTVals)]

makeShowModels :: forall a b.Eq a => (Formula a -> [a -> b]) -> Formula a -> [[(a, b)]]
makeShowModels mods formula =
    let atoms = atomsSet formula
    in  [ [ (a, m a) | a <- atoms ] | m <- mods formula ]




{- Todo:
    are there some sensible defaults for valid, unsat and sat,
    that hold in all possible semantics? (not only PC, LPC, ...)
    example for PC:
    -- negation of a tautology yields a contradiction
    valid = F.Not unsat
    -- negation of a contradiciton yields a tautology
    -- -> so its a satisfiable formula
    unsat = sat . F.Not
    valid = unsat . F.Not
    sat = to be implemented
-}


