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
    (
    -- * Types
      Semantics (..)
    , TrVals(..)
    , Property(Valid,Sat,Unsat)
    -- * Constructor functions
    , makeTrVals
    , makeSemantics
    -- * Prototype functions
    , protoDomain
    , protoModels
    , protoModelsLookup
    , protoSat
    , protoValid
    , protoEntails
    -- * Utility functions
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

-- | A container type for truth-values; combines the set of truth-values T and DT
-- (with DT ⊂ T) where DT are the set of designated truth-values.
-- TrVals is an abstract data type that should only be accesible through its
-- smart constructor makeTrVals.
data TrVals a = TrVals
    { getTrVals :: [a]
    , getDesigTrVals :: [a]
    } deriving (Show, Eq)

-- | Type for the class of many-valued semantics of propositional calculus.
data Semantics b = Semantics {
    -- | A set T of truth-values.
    trVals :: [b],
    -- | A set DT of designated truth-values.
    desigTrVals :: [b],
    -- | An evaluation function V : Formula T → Formula T
    -- that reduces Formula T of arbitrary length to a Formula T
    -- of structure 'Atom t' with t ∈ T.
    eval :: Formula b -> Formula b,
    -- | A function that maps a Formula Prop f to the set of its potential models PM.
    -- The assignment functions that are potential models of f have to be defined
    -- at least for every propositional variable p with p ∈ P(f).
    domain :: Formula Prop -> [Prop -> b],
    -- | A function that maps a Formula Prop f to the set of its models.
    -- The assignment functions that are models of of f have to be defined
    -- at least for every propositional variable p with p ∈ P(f).
    models :: Formula Prop -> [Prop -> b],
    -- | A function that protos the name-value-pairs of the models of a Formula Prop f explicit.
    -- The values returned by 'domain' and 'models' are 'raw' functions.
    modelsLookup :: Formula Prop -> [[(Prop,b)]],
    -- | A function that verifies validity of a Formula Prop f under a Semantics T.
    valid :: Formula Prop -> Bool,
    -- | A function that verifies satisfiability of a Formula Prop f under a Semantics T.
    sat :: Formula Prop -> Bool,
    -- | A function that verifies unsatisfiability of a Formula Prop f under a Semantics T.
    unsat :: Formula Prop -> Bool,
    -- | A function that verifies the entailment relation between a set of Formula Prop F
    -- and a Formula Prop f under a Semantics S T s.
    entails :: [Formula Prop] -> Formula Prop -> Bool
}

-- | Smart-constructor for truth-values.
-- It guarantees the following properties of every TrVal a:
-- 1) getDesigTrVals a ⊂ getTrVals a
makeTrVals :: Eq a => [a] -> [a] -> TrVals a
makeTrVals vals desigVals =
    case nub desigVals \\ nub vals of
    [] -> TrVals vals desigVals
    _ -> error "Error(protoTrVals): Set of truth values isn't superset of set of designated Truth values!"

-- | Constructor for a generic many-values propositional semantics Semantics T over truth-values T.
-- This constructor takes two arguments:
-- 1) A set of truth values V and designated truth values DV ⊂ V (combined in a type TrVals V).
-- 2) An evaluation function that reduces a f :: Formula V of arbitrary length to a formula of structure 'Atom t' with t ∈ T.
--    It has to be defined for the type constructors Atom a, Not a, And a, Or a, Imp a, Iff a of type Formula a.
makeSemantics :: Eq b =>
    TrVals b                            -- ^ The truth-value-Argument e.g. makeTrVals [T,F] [F] (assuming there is a data V = T | F)
    -> (Formula b -> Formula b)         -- ^ An evaluation formula for Formula V; it has to be defined for Atom, Not, And, Or, Imp, Iff.
    -> Semantics b                      -- ^ The generated Semantics.
makeSemantics tvs evalFn = Semantics
    { trVals = getTrVals tvs
    , desigTrVals = getDesigTrVals tvs
    , eval = evalFn
    , domain = protoDomain tvs
    , models = protoModels tvs evalFn
    , modelsLookup = protoModelsLookup tvs evalFn
    , unsat = not . protoSat tvs evalFn
    , sat = protoSat tvs evalFn
    , valid = protoValid tvs evalFn
    , entails = protoEntails tvs evalFn
    }

-- | This is a generic prototype for domain-functions for Formula Prop over truth-values T.
-- When given a set of truth-values T as an argument (wrapped in TrVals T)
-- it returns a function d: Formula Prop → Prop → T that generates all possible models/assignment
-- functions A: Prop → T for a Formula Prop f  based on the propositional variables in f.
-- d is only a partial function: it is undefined for propositional variables that don't appear in f.
protoDomain :: TrVals b -> Formula Prop -> [Prop -> b]
protoDomain tvs fm =
    protoAssignmentFn <$> lookupTables
  where protoAssignmentFn :: [(Prop, b)] -> Prop -> b
        protoAssignmentFn lookupTable a =
            let m = fromList lookupTable
            in fromMaybe (error "Error(Assignment): variable not in assignment function")
               (lookup a m)
        pairsAtomValue = cartProd (atomsSet fm) (getTrVals tvs)
        partitionsByAtom = groupBy ((==) `on` fst) pairsAtomValue
        lookupTables = combination partitionsByAtom

protoModels :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> [Prop -> b]
protoModels ts evalFn fm =
    let d = protoDomain ts fm
        filt = flip elem (Atom <$> getDesigTrVals ts)
        mask = map filt ((evalFn . ($ fm) . onAtoms) <$> d)
    in [ m | (m, True) <- d `zip` mask]

protoModelsLookup :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> [[(Prop, b)]]
protoModelsLookup ts evalFn fm =
    let as = atomsSet fm
        ms = protoModels ts evalFn fm
    in map (zip as . ($ as) . map) ms

protoSat :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> Bool
protoSat tvs evalFn fm = (not . null) (protoModels tvs evalFn fm)

protoValid :: Eq b => TrVals b -> (Formula b -> Formula b) -> Formula Prop -> Bool
protoValid tvs evalFn fm = length (protoModels tvs evalFn fm) == length (protoDomain tvs fm)

protoEntails :: Eq b =>
     TrVals b -> (Formula b -> Formula b) -> [Formula Prop] -> Formula Prop -> Bool
protoEntails tvs evalFn fms fm =
    let modelsFm = protoModelsLookup tvs evalFn fm
        allModelsFms = map (protoModelsLookup tvs evalFn) fms
        modelsFms = intersectModelLookups allModelsFms
        atomsInFm = nub $ concat $ (fmap . fmap) fst modelsFm
        atomsInFms =  nub $ concat $ (fmap . fmap) fst modelsFms
        atoms = nub $ atomsInFms ++ atomsInFm
        extModelsFm = sortModels $ concat $ fmap (extendModel tvs atoms) modelsFm
        extModelsFms = sortModels $ concat $ fmap (extendModel tvs atoms) modelsFms
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

-- | This function generates the combination of list of lists.
-- example:
-- combination [[1,2],[3,4]]
-- > [[1,3],[1,4],[2,3],[2,4]]
-- possible replacement: sequence xs
combination :: [[a]] -> [[a]]
combination [] = []
combination [as] = fmap (:[]) as
combination (x:xs) = (:) <$> x <*> combination xs

-- | This function generates the cartesian product of two lists.
-- example:
-- cartProd [1,2] ['a','b']
-- > [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
cartProd :: [a] -> [b] -> [(a,b)]
cartProd as bs = (,) <$> as <*> bs
