{-# OPTIONS_GHC -Wall -Werror #-}

module PropSemanticsLP
    ( V(F,I,T)
    , semantics
    ) where

import Prelude hiding (not, and, or, lookup, map)
import qualified Prelude as P (not)

import Semantics
import Formula (Formula(Atom,Not,And,Or,Imp,Iff))
import Prop

-- type of truth values
data V
    = F
    | I
    | T
    deriving (Show, Eq, Ord)

modelsLP :: Formula Prop -> [Prop -> V]
modelsLP = makeModels [T,I,F] [T,I] eval

eval :: Formula V -> Formula V
eval fm = case fm of
    (Atom T) -> Atom T
    (Atom I) -> Atom I
    (Atom F) -> Atom F
    (Not p) -> not (eval p)
    (And p q) -> and (eval p) (eval q)
    (Or p q) -> or (eval p) (eval q)
    (Imp p q) -> imp (eval p) (eval q)
    (Iff p q) -> iff (eval p) (eval q)
    _ -> error "Error(eval): undefined input"

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
imp (Atom p) aq@(Atom q) =
    case p of
    T -> aq
    I -> case q of
        T -> Atom T
        I -> Atom I
        F -> Atom I
    F -> Atom T
imp _ _ = undefined

iff :: Formula V -> Formula V -> Formula V
iff ap@(Atom _) aq@(Atom _) = imp ap aq `and` imp aq ap
iff _ _ = undefined


validLP :: Formula Prop -> Bool
validLP fm = length (modelsLP fm) == length (assignments [T,I,F] fm)

satLP :: Formula Prop -> Bool
satLP = P.not . unsatLP

unsatLP :: Formula Prop -> Bool
unsatLP = null . modelsLP

semantics :: Semantics Prop (Prop -> V)
semantics = Semantics {
    models = modelsLP,
    valid = validLP,
    sat = satLP,
    unsat = unsatLP,
    entails = undefined
}


