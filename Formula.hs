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
    | Forall (Formula a)
    | Exists (Formula a)
    deriving Show
