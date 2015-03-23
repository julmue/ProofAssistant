module PropSemanticsK3

where

import Prelude hiding (not, and, or, lookup)

import Control.Applicative ((<$>))
import Data.Map (fromList, lookup)
import Data.Function (on)
import Data.List (groupBy)
import Data.Maybe (fromMaybe)

import Semantics
import Formula (Formula(Atom,Not,And,Or,Imp,Iff),atomsSet,onAtoms)
import Prop
import Misc

-- type of truth values
data V
    = T
    | I
    | F
    deriving (Show, Eq, Ord)

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


modelsK3 :: Formula Prop -> [Prop -> V]
modelsK3 = makeModels [T,I,F] [T] eval

eval :: Formula V -> Formula V
eval fm = case fm of
    (Atom T) -> Atom T
    (Atom I) -> Atom I
    (Atom F) -> Atom F
    (Not p) -> not (eval p)
    (And p q) -> and (eval p) (eval q)
    (Or p q) -> or (eval p) (eval q)

not (Atom p) = case p of
    T  -> Atom F
    I  -> Atom I
    F  -> Atom T
not _ = undefined

and (Atom p) aq@(Atom q) =
    case p of
    T -> aq
    I -> case q of
        T -> Atom I
        I -> Atom I
        F -> Atom F
    F -> Atom F
and _ _ = undefined

or (Atom p) aq@(Atom q) =
    case p of
    T -> Atom T
    I -> case q of
        T -> Atom T
        I -> Atom I
        F -> Atom I
    F -> aq
or _ _ = undefined

-- imp (Atom p) (Atom q) =
    -- case


