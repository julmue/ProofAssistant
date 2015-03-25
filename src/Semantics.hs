-- {-# OPTIONS_GHC -Wall -Werror #-}

module Semantics
    ( Semantics (..)
    , TrVals(..)
    , Property(..)
    , makeTrVals
    , makeDomain
    , makeSemantics
    , fillUpModels
    , createModels
    , makeModels
    , makeModelsLookup
    , fillAll
    )
where

import Prelude hiding (lookup)

import Control.Applicative ((<$>), (<*>), pure, ZipList(..))
import Data.Function (on)
import Data.List ((\\), groupBy, sortBy, nub, intersect)
import Data.Map (fromList, lookup)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

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
protoEntails = undefined
-- protoEntails tvs evalFn fms fm =
--     let fmsModelsLup = concat $ fmap (makeModelsLookup tvs evalFn) fms
--         fmModelsLup = (makeModelsLookup tvs evalFn) fm
--         asInFmsMods = getAtoms fmsModelsLup
--         asInFmMods = getAtoms fmModelsLup
--         asNotInFm = asInFmsMods \\ asInFmMods
--         asNotInFms = asInFmMods \\ asInFmsMods
--         fillingFm = filling asNotInFm
--         fillingFms = filling asNotInFms
--         newFmsMods = newModelsLup fillingFms fmsModelsLup
--         newFmMods = newModelsLup fillingFm fmModelsLup
--     in (sortModels newFmsMods, sortModels newFmMods)
--     in case (sortModels newFmsMods) \\ (sortModels newFmMods) of
--         [] -> True
--         _ -> False
--   where
--     getAtoms f = nub $ fmap fst $ (nub . concat) f
--     filling atoms = fmap (,) atoms <*> (getTrVals tvs)
--     newModelsLup fillings models =
--         case fillings of
--         [] -> models
--         _ -> (fmap (:) fillings) <*> models
--     sortModels ms =
--         map (sortBy (\(x,_) (y,_) -> x `compare` y)) ms

{- function that calculates the set of all models of a given set of formulas
    1. Problem: not all Variables have to be in every Formula
        The models of each formula have to be "filled" with combinations
        of the atoms and truth values that for atoms that don't appear in
        the formula but elsewhere in the set
        Input: formula f, set of formulas fs (important: minus the formula itself)
        1.1 :
            -> calculate the set of all values embedded in atoms in a formula
            -> calculate the set of all atoms in a set of formulas

            get Atoms in Formula
                > af = atomsSet f
            get Atoms in set of fomula
                > afs = (concat . (fmap atomsSet)) fs

        1.2 : now the difference between two atom sets

            atoms appearing only in the formula
                > aof = af \\ afs -- these don't matter right now
            atoms appearing in only in the set
                > aofs = afs \\ af

        1.3 : create a list of fillings where each filling has to be appended to each
              model of the formula so that each variable in the set of formulas also
              appears in each model of the formula

              create fillings:
              fillings = sequence association aofs truthValues

              append fillings to the model
              (:) <$> fillings <*> ms

     -> for each formula and its models create a list of fillings of pairs (atom,Trv)
         [[(atom1,Tv1), ...] , [(atomn,Tv1), ...]

     -> then sort it
-}


association :: Functor f => f a -> [b] -> f [(a, b)]
association l1 l2 =
    fmap ($ l2) $ fmap (<*>) $ (fmap . fmap) (,) (fmap (pure) l1)

-- should guarantee that f is not in fs ... doesn't right now
fillUpModels :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Formula a -> [Formula a] -> [[(a, b)]]
fillUpModels tvs evalFn fm fms =
    let msFm =  (makeModelsLookup tvs evalFn) fm
        afm = atomsSet fm
        afms = (concat . (fmap atomsSet)) fms
        aofms = afms \\ afm
        in case aofms of
           [] -> msFm
           _ -> let fillings = sequence $ association aofms (getTrVals tvs)
                in (fmap (++) fillings) <*> msFm


createModels :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> [Formula a] -> [[(a, b)]]
createModels tvs evalFn fms = case fillAll tvs evalFn fms of
    [] -> []
    ms -> foldr1 intersect $ fmap sortModels ms

fillAll :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> [Formula a] -> [[[(a, b)]]]
fillAll tvs evalFn fms =
    fmap (\fm -> fillUpModels tvs evalFn fm (fms\\[fm])) fms

sortModels :: Ord a => [[(a, t)]] -> [[(a, t)]]
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


