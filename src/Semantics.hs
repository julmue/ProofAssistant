-- {-# OPTIONS_GHC -Wall -Werror #-}

module Semantics
    ( Semantics (..)
    , Property(..)
    , makeTrVals
    , makeDomain
    , makeSemantics
 --   , makeModels
 --   , makeModelsLookup
    )
where

import Prelude hiding (lookup)

import Control.Applicative ((<$>), (<*>), pure, ZipList(..))
import Data.Function (on)
import Data.List ((\\), groupBy, sortBy, nub)
import Data.Map (fromList, lookup)
import Data.Maybe (fromMaybe)

import Formula hiding (True, False)
import Misc

data Property
    = Valid
    | Sat
    | Unsat
    deriving (Show, Eq)

data TrVals a = TrVals
    { getTrVals :: [a]
    , getDesigTrVals :: [a]
    } deriving (Show, Eq)

makeTrVals :: Eq a => [a] -> [a] -> TrVals a
makeTrVals trVals desigTrVals =
    case (nub desigTrVals) \\ (nub trVals) of
    [] -> TrVals trVals desigTrVals
    _ -> error "Error(makeTrVals): Set of truth values isn't superset of set of designated Truth values!"

-- | class of finite, many-values semantics
data Semantics a b = Semantics {
    trVals :: [b],
    desigTrVals :: [b],
    eval :: Formula b -> Formula b,
    domain :: Formula a -> [a -> b],
    models :: Formula a -> [a -> b],
    modelsLookup :: Formula a -> [[(a,b)]],
    valid :: Formula a -> Bool,
    sat :: Formula a -> Bool,
    unsat :: Formula a -> Bool,
    entails :: [Formula a] -> Formula a -> ([[(a,b)]],[[(a,b)]])
}

makeSemantics :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Semantics a b
makeSemantics tvs evalFn = Semantics
    { trVals = getTrVals tvs
    , desigTrVals = getDesigTrVals tvs
    , eval = evalFn
    , domain = makeDomain tvs
    , models = makeModels tvs evalFn
    , modelsLookup = makeModelsLookup tvs evalFn
    , unsat = not . (protoSat tvs evalFn)
    , sat = protoSat tvs evalFn
    , valid = protoValid tvs evalFn
    , entails = protoEntails tvs evalFn
    }

protoSat :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Formula a -> Bool
protoSat tvs evalFn fm = (not . null) (makeModels tvs evalFn fm)

protoValid :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Formula a -> Bool
protoValid tvs evalFn fm = length (makeModels tvs evalFn fm) == length (makeDomain tvs fm)

protoEntails
  :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> [Formula a] -> Formula a ->  ([[(a,b)]],[[(a,b)]])
protoEntails tvs evalFn fms fm =
    let fmsModelsLup = concat $ fmap (makeModelsLookup tvs evalFn) fms
        fmModelsLup = (makeModelsLookup tvs evalFn) fm
        asInFmsMods = getAtoms fmsModelsLup
        asInFmMods = getAtoms fmModelsLup
        asNotInFm = asInFmsMods \\ asInFmMods
        asNotInFms = asInFmMods \\ asInFmsMods
        fillingFm = filling asNotInFm
        fillingFms = filling asNotInFms
        newFmsMods = newModelsLup fillingFms fmsModelsLup
        newFmMods = newModelsLup fillingFm fmModelsLup
    in (sortModels newFmsMods, sortModels newFmMods)
--     in case (sortModels newFmsMods) \\ (sortModels newFmMods) of
--         [] -> True
--         _ -> False
  where
    getAtoms f = nub $ fmap fst $ (nub . concat) f
    filling atoms = fmap (,) atoms <*> (getTrVals tvs)
    newModelsLup fillings models =
        case fillings of
        [] -> models
        _ -> (fmap (:) fillings) <*> models
    sortModels ms =
        map (sortBy (\(x,_) (y,_) -> x `compare` y)) ms





-- | function generates the subset of the domain of functions V : P -> TV
--   where possible models of a formula can stem from.
makeDomain :: Ord a => TrVals b -> Formula a -> [a -> b]
makeDomain tvs fm =
    makeAssignmentFn <$> lookupTables
  where makeAssignmentFn :: Ord a => [(a,b)] -> a -> b
        makeAssignmentFn lookupTable a =
            let m = fromList lookupTable
            in fromMaybe (error "Error(Assignment): variable not in assignment function")
               (lookup a m)
        pairsAtomValue = cartProd (atomsSet fm) (getTrVals tvs)
        partitionsByAtom = groupBy ((==) `on` fst) pairsAtomValue
        lookupTables = combination partitionsByAtom
                    -- sequence partitionsByAtom

makeModels :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Formula a -> [a -> b]
makeModels ts eval fm =
    let domain = makeDomain ts fm
        filt = (flip elem) (Atom <$> (getDesigTrVals ts))
        mask = map filt ((eval . ($ fm) . onAtoms) <$> domain)
    in [ m | (m, t) <- domain `zip` mask, t == True ]

makeModelsLookup :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Formula a -> [[(a, b)]]
makeModelsLookup ts eval fm =
    let as = atomsSet fm
        ms = makeModels ts eval fm
    in map (zip $ as) $ map ($ as) (map map ms)


