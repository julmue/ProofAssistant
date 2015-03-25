-- {-# OPTIONS_GHC -Wall -Werror #-}

module PropSemanticsPC
    ( V(F,T)
--    , semantics
    ) where

import Prelude hiding (not, and, or, lookup)
import qualified Prelude as P (not)

import Semantics
import Formula (Formula(Atom,Not,And,Or,Imp,Iff))
import Prop
import Debug.Trace (trace)

data V
    = F
    | T
    deriving (Show, Ord, Eq)

-- models:
-- the set of all PC models of a formula f
-- modelsPC :: Formula Prop -> [Prop -> V]
-- modelsPC = makeModels semantics

trvPC = makeTrVals [T,F] [T]

-- evaluation function
evalPC :: Formula V -> Formula V
evalPC fm = case fm of
    (Atom T) -> Atom T
    (Atom F) -> Atom F
    (Not p) -> not (evalPC p)
    (And p q) -> and (evalPC p) (evalPC q)
    (Or p q) -> or (evalPC p) (evalPC q)
    (Imp p q) -> imp (evalPC p) (evalPC q)
    (Iff p q) -> iff (evalPC p) (evalPC q)
    _ -> error "Error(eval): undefined input"

-- truth functions
not :: Formula V -> Formula V
not (Atom p) = case p of
    T -> Atom F
    F -> Atom T
not _ = undefined

and :: Formula V -> Formula V -> Formula V
and (Atom p) aq@(Atom _) =
    case p of
    T -> aq
    F -> Atom F
and _ _ = undefined

or :: Formula V -> Formula V -> Formula V
or (Atom p) aq@(Atom _) =
    case p of
    T -> Atom T
    F -> aq
or _ _ = undefined

imp :: Formula V -> Formula V -> Formula V
imp (Atom p) (Atom q) =
    case p of
    T -> case q of
        T -> Atom T
        F -> Atom F
    F -> Atom T
imp _ _ = undefined

iff :: Formula V -> Formula V -> Formula V
iff ap@(Atom _) aq@(Atom _) = imp ap aq `and` imp aq ap
iff _ _ = undefined

semantics :: Semantics Prop V
semantics = makeSemantics trvPC evalPC


-- checks if a formula is valid / a tautology
-- a formula is valid iff its negation is unsatisfiable
-- validPC :: Formula Prop -> Bool
-- validPC = unsatPC . Not

-- checks if formula is sat / a contingent formula
-- satPC :: Formula Prop -> Bool
-- satPC = P.not . unsatPC

-- checks if formula is not sat / a contradiction
-- unsatPC :: Formula Prop -> Bool
-- unsatPC = null . modelsPC

-- entailsPC :: [Formula Prop] -> Formula Prop -> Bool
-- entailsPC fs f = case fs of
--    [] -> validPC f
--    _ -> validPC $ Imp (fullAnd fs) f


-- this should definitly be in the Formula Module!
fullAnd :: [Formula a] -> Formula a
fullAnd fs = case fs of
    []  -> error "Error(fullAnd): empty list of Formulas"
    [f] -> f
    (hf:tf) -> And hf $ fullAnd tf

-- semantics :: Semantics Prop (Prop -> V)
-- semantics = Semantics {
--     eval = evalPC,
--     models = modelsPC,
--     valid = validPC,
--     sat = satPC,
--     unsat = unsatPC,
--     entails = entailsPC,
--     trVals = [T,F],
--     desigTrVals = [T]
--}


-- setModels :: [Formula Prop] -> [Prop -> V]
-- setModels = concat . (fmap modelsPC)


-- entailment fms fm = and fmap ('elem' ) allModels fm
