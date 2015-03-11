module Formula (Formula (..), 
                Quantifier (..)) 
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
    | Quantification Quantifier (Formula a)
    deriving Show

data Quantifier 
    = Forall 
    | Exists
    deriving Show 
