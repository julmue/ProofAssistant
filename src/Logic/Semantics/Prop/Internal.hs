{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Semantics.Internal
-- Description : Type
-- Copyright   : (c) Julian Müller, 2013
-- License     : GPL-3
-- Maintainer  : jul.mue@hotmail.de
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the internal interface of the Semantics module.
-- It exports the semantics data type, as well as some functions to
-- generate finite, many-valued by specifiying some truth values
-- and an evaluation function.
--

module Logic.Semantics.Prop.Internal
    ( Semantics (..)
    , TrVals(..)
    , Property(..)
    , makeTrVals
    , makeSemantics
    , makeDomain
    , makeModels
    , makeModelsLookup
    , protoSat
    , protoValid
    , protoEntails
    , intersectModelLookups
    , extendModel
    , sortModels
    , association
    , combination
    , cartProd
    )
where

import Prelude hiding (lookup)

import Control.Applicative ((<$>), (<*>), pure)
import Data.Function (on)
import Data.List ((\\), groupBy, sortBy, nub, intersect)
import Data.Map (fromList, lookup)
import Data.Maybe (fromMaybe)

import Logic.Data.Formula hiding (True, False)
import Logic.Data.Prop

-- | The type of properties a given formula f can have with respect
-- to a semantics S: f is valid in S, f is satisfiable in S or f is unsatisfiable in S.
data Property
    = Valid
    | Sat
    | Unsat
    deriving (Show, Eq)

-- | The type of truth-values V that are the codomain of the set of
-- functions F: A -> V
data TrVals a = TrVals
    { getTrVals :: [a]
    , getDesigTrVals :: [a]
    } deriving (Show, Eq)

-- | Type for Class of many-valued Semantics of propositional Calculus-

-- Question:
-- Is it a Class for Formulas over Prop only?
-- consisting of a triple (A, F1: A->B, F2: B -> B)
-- where A: Prop
--       B: functions from Prop -> TV
--       C: Reduction/Evaluation function Formula B -> Formula B
-- then the Type a can be dropped
-- if not what other semantics are possible?

-- Possible extension:
-- what is with the type Term and a function e': Term a -> Formula b?
-- I think this is alright
data Semantics b = Semantics {
    trVals :: [b],
    desigTrVals :: [b],
    eval :: Formula b -> Formula b,
    domain :: Formula Prop -> [Prop -> b],
    models :: Formula Prop -> [Prop -> b],
    modelsLookup :: Formula Prop -> [[(Prop,b)]],
    valid :: Formula Prop -> Bool,
    sat :: Formula Prop -> Bool,
    unsat :: Formula Prop -> Bool,
    entails :: [Formula Prop] -> Formula Prop -> Bool
}

makeTrVals :: Eq a => [a] -> [a] -> TrVals a
makeTrVals vals desigVals =
    case nub desigVals \\ nub vals of
    [] -> TrVals vals desigVals
    _ -> error "Error(makeTrVals): Set of truth values isn't superset of set of designated Truth values!"

makeSemantics :: Eq b => TrVals b -> (Formula b -> Formula b) -> Semantics b
makeSemantics tvs evalFn = Semantics
    { trVals = getTrVals tvs
    , desigTrVals = getDesigTrVals tvs
    , eval = evalFn
    , domain = makeDomain tvs
    , models = makeModels tvs evalFn
    , modelsLookup = makeModelsLookup tvs evalFn
    , unsat = not . protoSat tvs evalFn
    , sat = protoSat tvs evalFn
    , valid = protoValid tvs evalFn
    , entails = protoEntails tvs evalFn
    }

-- | function generates the subset of the domain of functions V : P -> TV
--   where possible models of a formula can stem from.
makeDomain :: TrVals b -> Formula Prop -> [Prop -> b]
makeDomain tvs fm =
    makeAssignmentFn <$> lookupTables
  where makeAssignmentFn :: [(Prop, b)] -> Prop -> b
        makeAssignmentFn lookupTable a =
            let m = fromList lookupTable
            in fromMaybe (error "Error(Assignment): variable not in assignment function")
               (lookup a m)
        pairsAtomValue = cartProd (atomsSet fm) (getTrVals tvs)
        partitionsByAtom = groupBy ((==) `on` fst) pairsAtomValue
        lookupTables = combination partitionsByAtom
                    -- sequence partitionsByAtom

makeModels :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> [Prop -> b]
makeModels ts evalFn fm =
    let d = makeDomain ts fm
        filt = flip elem (Atom <$> getDesigTrVals ts)
        mask = map filt ((evalFn . ($ fm) . onAtoms) <$> d)
    in [ m | (m, True) <- d `zip` mask]

makeModelsLookup :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> [[(Prop, b)]]
makeModelsLookup ts evalFn fm =
    let as = atomsSet fm
        ms = makeModels ts evalFn fm
    in map (zip as . ($ as) . map) ms

protoSat :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> Bool
protoSat tvs evalFn fm = (not . null) (makeModels tvs evalFn fm)

protoValid :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> Bool
protoValid tvs evalFn fm = length (makeModels tvs evalFn fm) == length (makeDomain tvs fm)

protoEntails :: Eq b =>
     TrVals b -> (Formula b -> Formula b) -> [Formula Prop] -> Formula Prop -> Bool --  [[[(a, b)]]]
protoEntails tvs evalFn fms fm =
    let modelsFm = makeModelsLookup tvs evalFn fm
        allModelsFms = map (makeModelsLookup tvs evalFn) fms
        modelsFms = intersectModelLookups allModelsFms
        atomsInFm = nub $ concat $ (fmap . fmap) fst modelsFm
        atomsInFms =  nub $ concat $ (fmap . fmap) fst modelsFms
        atoms = nub $ atomsInFms ++ atomsInFm
        extModelsFm = sortModels $ concat $ fmap (extendModel tvs atoms) modelsFm
        extModelsFms = sortModels $ concat $ fmap (extendModel tvs atoms) modelsFms
--    in  [extModelsFms, extModelsFm,extModelsFms\\extModelsFm]
    in case extModelsFms\\extModelsFm of
        [] -> True
        _ -> False

intersectModelLookups :: Eq b => [[[(Prop, b)]]] -> [[(Prop, b)]]
intersectModelLookups ms = case ms of
     [] -> []
     x -> foldr1 intersect $ fmap sortModels x

extendModel :: TrVals b -> [Prop] -> [(Prop, b)] -> [[(Prop, b)]]
extendModel tvs atoms mlookups =
    let as = nub atoms
        msAtoms = nub $ fmap fst mlookups
        atomsOnly = as \\ msAtoms
        extensions = sequence $ association atomsOnly (getTrVals tvs)
    in  fmap(mlookups ++) extensions

sortModels :: [[(Prop, b)]] -> [[(Prop, b)]]
sortModels =
    map (sortBy (\(x,_) (y,_) -> x `compare` y))

association :: Functor f => f Prop -> [b] -> f [(Prop, b)]
association l1 l2 =
    fmap (($ l2) . (<*>) . fmap (,) . pure) l1

-- combination of sets
-- all possible combinations of single element from every set.
-- whats the exact name of the thing?
combination :: [[a]] -> [[a]]
combination [] = []
combination [as] = fmap (:[]) as
combination (x:xs) = (:) <$> x <*> combination xs

-- crossproduct
cartProd :: [a] -> [b] -> [(a,b)]
cartProd as bs = (,) <$> as <*> bs
