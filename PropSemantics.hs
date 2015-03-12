module PropSemantics 
    (   eval 
    ,   assignments
    ,   models
    ,   valid
    ,   sat
    ,   unsat
    )
where 

import Control.Applicative

import qualified Data.Map as M
import Data.List
import Data.Function (on)
import Data.Maybe (fromMaybe)

import qualified Formula as F
import qualified Prop as P

-- evaluation function of Formula over domain a 
eval :: F.Formula P.Prop -> (P.Prop -> Bool) -> Bool
eval fm v = case fm of
    F.False     -> False
    F.True      -> True
    F.Atom x    -> v x
    F.Not p     -> not $ eval p v
    F.And p q   -> eval p v && eval q v
    F.Or p q    -> eval p v || eval q v
    F.Imp p q   -> not (eval p v) || eval q v
    F.Iff p q   -> eval p v == eval q v


-- models:
-- the set of all models of a formula f

-- assignments:
-- set of possible assignment functions 
-- based on the propositional variables in a logic formula
assignments :: F.Formula P.Prop -> [P.Prop -> Bool]
assignments fm = 
    let atoms = F.atomsSet fm
        prod = cartP atoms [True,False] 
        eqp = groupBy ((==) `on` fst) prod
    in assignTemplate <$> combine eqp
    where assignTemplate :: [(P.Prop,Bool)] -> P.Prop -> Bool
          assignTemplate list prop =  
                let map = M.fromList list
                in fromMaybe (error "variable not in assignment function") 
                    (M.lookup prop map)  

models :: F.Formula P.Prop -> [P.Prop -> Bool]
models fm = 
    let assgn = assignments fm
        mask = eval fm <$> assgn
    in [ x | (x,True) <- zip assgn mask]

-- checks if a formula is valid / a tautology
-- a formula is valid iff its negation is unsatisfiable
valid :: F.Formula P.Prop -> Bool
valid = unsat . F.Not

-- checks if formula is sat / a contingent formula
sat :: F.Formula P.Prop -> Bool
sat = not . unsat

-- checks if formula is not sat / a contradiction
unsat :: F.Formula P.Prop -> Bool
unsat = null . models


{- helper functions -}
-- crossproduct
cartP :: [a] -> [b] -> [(a,b)]
cartP as bs = (,) <$> as <*> bs

-- combination of two sets
-- whats the exact name of the thing?
combine :: [[a]] -> [[a]]
combine [as] = fmap (:[]) as
combine (x:xs) = (:) <$> x <*> combine xs
