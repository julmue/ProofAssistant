{-# OPTIONS_GHC -Wall -Werror #-}

module PropSemanticsRM
    ( V(F,I,T)
    , semantics
    ) where

import Prelude hiding (not, and, or, lookup, map)

import Semantics
import Formula (Formula(Atom,Not,And,Or,Imp,Iff))
import Prop

-- type of truth values
data V
    = F
    | I
    | T
    deriving (Show, Eq, Ord)

trvRM :: TrVals V
trvRM = makeTrVals [T,I,F] [T,I]

evalRM :: Formula V -> Formula V
evalRM fm = case fm of
    (Atom T) -> Atom T
    (Atom I) -> Atom I
    (Atom F) -> Atom F
    (Not p) -> not (evalRM p)
    (And p q) -> and (evalRM p) (evalRM q)
    (Or p q) -> or (evalRM p) (evalRM q)
    (Imp p q) -> imp (evalRM p) (evalRM q)
    (Iff p q) -> iff (evalRM p) (evalRM q)
    _ -> error "Error(evalRM): undefined input"

not :: Formula V -> Formula V
not (Atom p) = case p of
    T  -> Atom F
    I  -> Atom I
    F  -> Atom T
not _ = undefined

and :: Formula V -> Formula V -> Formula V
and (Atom p) aq@(Atom q) =
    case p of
    T -> aq
    I -> case q of
        T -> Atom I
        I -> Atom I
        F -> Atom F
    F -> Atom F
and _ _ = undefined

or :: Formula V -> Formula V -> Formula V
or (Atom p) aq@(Atom q) =
    case p of
    T -> Atom T
    I -> case q of
        T -> Atom T
        I -> Atom I
        F -> Atom I
    F -> aq
or _ _ = undefined

imp :: Formula V -> Formula V -> Formula V
imp (Atom p) (Atom q) =
    case p of
    T -> case q of
        T -> Atom T
        _ -> Atom F
    I -> case q of
        T -> Atom T
        I -> Atom I
        F -> Atom I
    F -> Atom T
imp _ _ = undefined

iff :: Formula V -> Formula V -> Formula V
iff ap@(Atom _) aq@(Atom _) = imp ap aq `and` imp aq ap
iff _ _ = undefined

semantics :: Semantics Prop V
semantics = makeSemantics trvRM evalRM
