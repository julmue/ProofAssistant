module Formula 
    (Formula (..)) 
where

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
    deriving Show


-- constructor functions
mk_and :: (Formula a) -> (Formula a) -> (Formula a) 
mk_and p q = And p q

mk_or :: (Formula a) -> (Formula a) -> (Formula a)
mk_or p q = Or p q

mk_imp :: (Formula a) -> (Formula a) -> (Formula a)
mk_imp p q = Imp p q

mk_iff :: (Formula a) -> (Formula a) -> (Formula a)
mk_iff p q = Iff p q

mk_forall :: String -> (Formula a) -> (Formula a)
mk_forall s p = Forall s p

mk_exists :: String -> (Formula a) -> (Formula a)
mk_exists s p = Exists s p

-- destructor functions
dest_iff :: (Formula a) -> ((Formula a),(Formula a))
dest_iff fm = case fm of
    (Iff p q)   -> (p,q)
    _           -> error "dest_iff: argument not an Iff"

antecedent :: (Formula a) -> (Formula a)
antecedent fm = case fm of
    (Iff p _)   -> p
    _           -> error "antecedent: argument not an IFF"

consequent :: (Formula a) -> (Formula a)
consequent fm = case fm of
    (Iff _ q)   -> q
    _           -> error "antecedent: argument not an IFF"

dest_and :: (Formula a) -> ((Formula a),(Formula a))
dest_and fm = case fm of
    (And p q)   -> (p,q)
    _           -> error "dest_iff: argument not and And"

conjuncts :: (Formula a) -> [(Formula a)]
conjuncts fm = case fm of
    (And p q)   -> conjuncts p ++ conjuncts q
    _           -> [fm]

dest_or :: (Formula a) -> ((Formula a),(Formula a))
dest_or fm = case fm of
    (Or p q)   -> (p,q)
    _           -> error "dest_iff: argument not and Or"

disjuncts :: (Formula a) -> [(Formula a)]
disjuncts fm = case fm of
    (Or p q)   -> disjuncts p ++ disjuncts q
    _           -> [fm]

-- generic recursive functions over formula

-- apply function to all atoms of formula but leave structure untouched
-- ?? are formulas Functors ?? this looks like fmap ... ??
onAtoms :: (a -> b) -> (Formula a) -> (Formula b)
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
-- overAtoms :: (a -> b -> b) -> (Formula a) -> b -> b 
overAtoms f fm b = case fm of
    Atom a      -> f a b
    Not p       -> overAtoms f p b
    And p q     -> overAtoms f p (overAtoms f q b) 
    Or p q      -> overAtoms f p (overAtoms f q b)
    Imp p q      -> overAtoms f p (overAtoms f q b)
    Iff p q     -> overAtoms f p (overAtoms f q b)
    Forall x p  -> overAtoms f p b
    Exists x p  -> overAtoms f p b

-- build the set of all atoms in a Formula
atomsSet :: Formula a -> [a]
atomsSet fm = overAtoms (:) fm []





-- Typeclasses of Formula
instance Functor Formula where
    fmap = onAtoms

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
