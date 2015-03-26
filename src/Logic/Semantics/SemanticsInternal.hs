{-# OPTIONS_GHC -Wall -Werror #-}

module Logic.Semantics.SemanticsInternal
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

data Property
    = Valid
    | Sat
    | Unsat
    deriving (Show, Eq)

data TrVals a = TrVals
    { getTrVals :: [a]
    , getDesigTrVals :: [a]
    } deriving (Show, Eq)

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
    entails :: [Formula a] -> Formula a -> Bool -- [[[(a,b)]]]
}

makeTrVals :: Eq a => [a] -> [a] -> TrVals a
makeTrVals vals desigVals =
    case nub desigVals \\ nub vals of
    [] -> TrVals vals desigVals
    _ -> error "Error(makeTrVals): Set of truth values isn't superset of set of designated Truth values!"

makeSemantics :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Semantics a b
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
makeModels ts evalFn fm =
    let d = makeDomain ts fm
        filt = flip elem (Atom <$> getDesigTrVals ts)
        mask = map filt ((evalFn . ($ fm) . onAtoms) <$> d)
    in [ m | (m, True) <- d `zip` mask]

makeModelsLookup :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Formula a -> [[(a, b)]]
makeModelsLookup ts evalFn fm =
    let as = atomsSet fm
        ms = makeModels ts evalFn fm
    in map (zip as . ($ as) . map) ms

protoSat :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Formula a -> Bool
protoSat tvs evalFn fm = (not . null) (makeModels tvs evalFn fm)

protoValid :: (Ord a, Eq b) => TrVals b -> (Formula b -> Formula b) -> Formula a -> Bool
protoValid tvs evalFn fm = length (makeModels tvs evalFn fm) == length (makeDomain tvs fm)

protoEntails :: (Ord a, Eq b) =>
     TrVals b -> (Formula b -> Formula b) -> [Formula a] -> Formula a -> Bool --  [[[(a, b)]]]
protoEntails tvs evalFn fms fm =
    let modelsFm = makeModelsLookup tvs evalFn fm               -- :: [[(a, b)]]
        allModelsFms = map (makeModelsLookup tvs evalFn) fms    -- :: [[[(a, b)]]]
        modelsFms = intersectModelLookups allModelsFms          -- :: [[(a, b)]]
        atomsInFm = nub $ concat $ (fmap . fmap) fst modelsFm
        atomsInFms =  nub $ concat $ (fmap . fmap) fst modelsFms
        atoms = nub $ atomsInFms ++ atomsInFm
        extModelsFm = sortModels $ concat $ fmap (extendModel tvs atoms) modelsFm
        extModelsFms = sortModels $ concat $ fmap (extendModel tvs atoms) modelsFms
--    in  [extModelsFms, extModelsFm,extModelsFms\\extModelsFm]
    in case extModelsFms\\extModelsFm of
        [] -> True
        _ -> False

intersectModelLookups :: (Ord a, Eq b) => [[[(a, b)]]] -> [[(a, b)]]
intersectModelLookups ms = case ms of
     [] -> []
     x -> foldr1 intersect $ fmap sortModels x

extendModel :: Eq a => TrVals b -> [a] -> [(a, b)] -> [[(a, b)]]
extendModel tvs atoms mlookups =
    let as = nub atoms
        msAtoms = nub $ fmap fst mlookups
        atomsOnly = as \\ msAtoms
        extensions = sequence $ association atomsOnly (getTrVals tvs)
    in  fmap(mlookups ++) extensions

sortModels :: Ord a => [[(a, t)]] -> [[(a, t)]]
sortModels =
    map (sortBy (\(x,_) (y,_) -> x `compare` y))

association :: Functor f => f a -> [b] -> f [(a, b)]
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
