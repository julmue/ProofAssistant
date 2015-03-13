module Formula 
    ( Formula (..)
    , mkAnd
    , mkOr
    , mkImp
    , mkForall
    , mkExists
    , destIff
    , antecedent
    , consequent
    , destAnd
    , conjuncts
    , destOr
    , disjuncts
    , onAtoms
    , overAtoms
    , atomsSet
    , simSubs
    , seqSubs
    ) 
where

import PrettyPrint
import Data.List

data Formula a 
    = False
    | True
    | Atom a
    | Not (Formula a)
    | And (Formula a) (Formula a)
    | Or (Formula a) (Formula a)
    | Imp (Formula a) (Formula a)
    | Iff (Formula a) (Formula a)
    | Forall String (Formula a)
    | Exists String (Formula a)
    deriving (Show, Eq)


-- constructor functions
mkAnd :: Formula a -> Formula a -> Formula a 
mkAnd = And

mkOr :: Formula a -> Formula a -> Formula a
mkOr = Or

mkImp :: Formula a -> Formula a -> Formula a
mkImp = Imp

mkIff :: Formula a -> Formula a -> Formula a
mkIff = Iff

mkForall :: String -> Formula a -> Formula a
mkForall = Forall

mkExists :: String -> Formula a -> Formula a
mkExists = Exists

-- destructor functions
destIff :: Formula a -> (Formula a,Formula a)
destIff fm = case fm of
    (Iff p q)   -> (p,q)
    _           -> error "dest_iff: argument not an Iff"

antecedent :: Formula a -> Formula a
antecedent fm = case fm of
    (Iff p _)   -> p
    _           -> error "antecedent: argument not an IFF"

consequent :: Formula a -> Formula a
consequent fm = case fm of
    (Iff _ q)   -> q
    _           -> error "antecedent: argument not an IFF"

destAnd :: Formula a -> (Formula a,Formula a)
destAnd fm = case fm of
    (And p q)   -> (p,q)
    _           -> error "dest_iff: argument not and And"

conjuncts :: Formula a -> [Formula a]
conjuncts fm = case fm of
    (And p q)   -> conjuncts p ++ conjuncts q
    _           -> [fm]

destOr :: Formula a -> (Formula a,Formula a)
destOr fm = case fm of
    (Or p q)   -> (p,q)
    _           -> error "dest_iff: argument not and Or"

disjuncts :: Formula a -> [Formula a]
disjuncts fm = case fm of
    (Or p q)   -> disjuncts p ++ disjuncts q
    _           -> [fm]

-- generic recursive functions over formula

-- apply function to all atoms of formula but leave structure untouched
-- ?? are formulas Functors ?? this looks like fmap ... ??
onAtoms :: (a -> b) -> Formula a -> Formula b
onAtoms f fm = case fm of
    Atom a      -> Atom $ f a
    Not p       -> Not $ onAtoms f p
    And p q     -> And (onAtoms f p) (onAtoms f q)
    Or p q      -> Or (onAtoms f p) (onAtoms f q)
    Imp p q     -> Imp (onAtoms f p) (onAtoms f q)
    Iff p q     -> Iff (onAtoms f p) (onAtoms f q)
    Forall x p  -> Forall x $ onAtoms f p
    Exists x p  -> Forall x $ onAtoms f p
--    _           -> fm that necessary? destroys (a -> b) and Functor behaviour

-- overatoms
overAtoms :: (a -> b -> b) -> Formula a -> b -> b 
overAtoms f fm b = case fm of
    Atom a      -> f a b
    Not p       -> overAtoms f p b
    And p q     -> overAtoms f p (overAtoms f q b) 
    Or p q      -> overAtoms f p (overAtoms f q b)
    Imp p q     -> overAtoms f p (overAtoms f q b)
    Iff p q     -> overAtoms f p (overAtoms f q b)
    Forall x p  -> overAtoms f p b
    Exists x p  -> overAtoms f p b

-- build the set of all atoms in a Formula
atomsSet :: Eq a => Formula a -> [a]
atomsSet fm = nub $  overAtoms (:) fm []

{- substitution -}

-- simultaneous substitution
-- if more than one substitution is specified for the same atom
-- the first one in the list is applied
-- simSub :: [(Formula a, Formula a)] -> Formula a -> Formula a
simSubs subList fm = case fm of 
    atom@(Atom _)   -> let substitutions = [ subs | subs@(fst,_) <- subList, fst == atom ]
                       in if null substitutions
                          then atom
                          else snd . head $ substitutions
    Not p           -> Not $ simSubs subList p
    And p q         -> And (simSubs subList p) (simSubs subList q)
    Or p q          -> Or (simSubs subList p) (simSubs subList q)
    Imp p q         -> Imp (simSubs subList p) (simSubs subList q)
    Iff p q         -> Iff (simSubs subList p) (simSubs subList q)
    Forall x p      -> Forall x $ simSubs subList p
    Exists x p      -> Exists x $ simSubs subList p
    
seqSubs :: Eq a => [(Formula a, Formula a)] -> Formula a -> Formula a
seqSubs (sub:subs) fm = let fm' = simSubs [sub] fm in seqSubs subs fm'
seqSubs [] fm = fm


{- typeclass instances of Formula -}
instance Functor Formula where
    fmap = onAtoms

instance PrettyPrint a => PrettyPrint (Formula a) where
    -- prettyPrint Formula a
    prettyPrint fm = case fm of
        Atom a      -> braces $ prettyPrint a
        Not p       -> braces $ "~" ++ prettyPrint p
        And p q     -> braces $ prettyPrint p ++ "&&" ++ prettyPrint q
        Or p q      -> braces $ prettyPrint p ++ "||" ++ prettyPrint q
        Imp p q     -> braces $ prettyPrint p ++ "->" ++ prettyPrint q
        Iff p q     -> braces $ prettyPrint p ++ "<->" ++ prettyPrint q
        Forall x p  -> braces $ "forall " ++ show x ++ ":" ++ prettyPrint p 
        Exists x p  -> braces $ "exists " ++ show x ++ ":" ++ prettyPrint p
        where braces a = "(" ++ a ++ ")"
 

-- functor law 1:
-- fmap id = id

-- functor law 2:
-- fmap (g . h) = (fmap g) . (fmap h)

-- maybe formula is an applicative functor ..
-- formula is a tree ...
-- conjunction of applicaton from (Formula (a->b)) to (Formula a)
-- instance Applicative Formula where
    -- pure :: a -> f a
    -- pure a = Atom a
    -- <*> :: f (a -> b) -> 
