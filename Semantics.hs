module Semantics
    ( Semantics (..)
    , assignments
    , makeModels
    )
where

import Formula as F
import Prelude as Pl

-- class of semantics for a

data Semantics a b = Semantics {
    isModel :: F.Formula a -> b -> Pl.Bool,
    models :: F.Formula a -> [b],
    valid :: F.Formula a -> Pl.Bool,
    sat :: F.Formula a -> Pl.Bool,
    unsat :: F.Formula a -> Pl.Bool
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
    where makeAssignment :: Ord a => [(a,b)] -> (a -> b)
          makeAssignment assnmtLookupTable a =
            let map = fromList assnmtLookupTable
            in fromMaybe (error "Error(Assignment): variable not in assignment function")
               (lookup a map)

-- makeModels :: (Ord a, Eq a) => [a] -> [a] -> (Formula a -> Formula a) -> Formula a -> [a -> a]
makeModels truthValues distinguishedTVals evalFn formula =
   let assnmts = assignments truthValues formula
       mask = evalFn <$> (sequence (onAtoms <$> (assignments truthValues formula)) formula)
    in [ model | (model, tValue) <- zip assnmts mask, tValue `elem`(Atom <$> distinguishedTVals)]


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


