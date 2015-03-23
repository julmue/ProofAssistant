module PropSemanticsK3

where

import Prelude hiding (not, and, or, lookup)

import Semantics
import Formula (Formula(Atom,Not,And,Or,Imp,Iff),atomsSet,onAtoms)
import Prop

-- type of truth values
data V
    = F
    | I
    | T
    deriving (Show, Eq, Ord)

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


