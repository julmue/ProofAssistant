module PropSemanticsPC
    ( pc
    )
where

import Prelude hiding (not, and, or, lookup)
import qualified Prelude as P (not)

import Semantics
import Formula hiding (True, False)
import Prop
import Misc

data V
    = F
    | T
    deriving (Show, Ord, Eq)

-- models:
-- the set of all PC models of a formula f
modelsPC :: Formula Prop -> [Prop -> V]
modelsPC = makeModels [T,F] [T] evalPC

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

-- truth functions
not (Atom p) = case p of
    T -> Atom F
    F -> Atom T
not _ = undefined

and (Atom p) aq@(Atom q) =
    case p of
    T -> aq
    F -> Atom F
and _ _ = undefined

or (Atom p) aq@(Atom q) =
    case p of
    T -> Atom T
    F -> aq
or _ _ = undefined

imp (Atom p) (Atom q) =
    case p of
    T -> case q of
        T -> Atom T
        F -> Atom F
    F -> Atom T
imp _ _ = undefined

iff ap@(Atom p) aq@(Atom q) = imp ap aq `and` imp aq ap
iff _ _ = undefined


-- checks if a formula is valid / a tautology
-- a formula is valid iff its negation is unsatisfiable
validPC :: Formula Prop -> Bool
validPC = unsatPC . Not

-- checks if formula is sat / a contingent formula
satPC :: Formula Prop -> Bool
satPC = P.not . unsatPC

-- checks if formula is not sat / a contradiction
unsatPC :: Formula Prop -> Bool
unsatPC = null . modelsPC

pc = Semantics {
    models = modelsPC,
    valid = validPC,
    sat = satPC,
    unsat = unsatPC
}
