module PropSemantics 
    (   eval
    ,   assignments
    ,   models
    ,   valid
    ,   satisfiable
 --   ,   unsatisfiable
    )
where 

import Control.Applicative

import qualified Data.Map as M
import Data.List

import qualified Formula as F
import qualified Prop as P

-- evaluation function of Formula over domain a 
eval :: F.Formula P.Prop -> (P.Prop -> Bool) -> Bool
eval fm v = case fm of
    F.False     -> False
    F.True      -> True
    F.Atom x    -> v x
    F.Not p     -> not $ eval p v
    F.And p q   -> (eval p v) && (eval q v)
    F.Or p q    -> (eval p v) || (eval q v)
    F.Imp p q   -> (not $ eval p v) || (eval q v)
    F.Iff p q   -> (eval p v) == (eval q v)


-- models:
-- the set of all models of a formula f

-- evals :: (F.Formula P.Prop) -> [(P.Prop -> Bool)]
-- set of possible assignment functions 
-- based on the propositional variables in a logic formula
assignments :: F.Formula P.Prop -> [P.Prop -> Bool]
assignments fm = 
    let atoms = F.atomsSet fm
        prod = cartP atoms [True,False] 
        eqp = groupBy (\x y -> fst x == fst y) prod
    in assignTemplate <$> (combine eqp)
    where assignTemplate :: [(P.Prop,Bool)] -> P.Prop -> Bool
          assignTemplate = \list prop ->  
                let map = M.fromList list
                in case M.lookup prop map of
                    Nothing -> error "variable not in assigment function"
                    Just b  -> b   

models :: F.Formula P.Prop -> [P.Prop -> Bool]
models fm = 
    let ass = assignments fm
        mask = (eval fm) <$> ass
    in [ x | (x,True) <- zipWith (,) ass mask]


valid :: F.Formula P.Prop -> Bool
valid fm =
    ((2^) $ length $ F.atomsSet fm) == (length $ models fm)


satisfiable :: F.Formula P.Prop -> Bool
satisfiable = not . unsatisfiable

unsatisfiable :: F.Formula P.Prop -> Bool
unsatisfiable = null . models

-- crossproduct
cartP :: [a] -> [b] -> [(a,b)]
cartP as bs = (,) <$> as <*> bs

-- combination of two sets
-- whats the exact name of the thing?
combine :: [[a]] -> [[a]]
combine [as] = fmap (\x -> [x]) as
combine (x:xs) = (:) <$> x <*> combine xs
