module PropSemantics 
    ( eval
    )
where 

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

