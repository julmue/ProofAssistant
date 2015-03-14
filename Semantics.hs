module Semantics
    (Semantics (..)
    )
where

import Formula as F
import Prelude as Pl

-- class of semantics for a

data Semantics a b = Semantics {
    isModel :: F.Formula a -> b -> Pl.Bool,
    models :: F.Formula a -> [b],
    valid :: F.Formula a -> Pl.Bool,
    sat :: F.Formula a -> Pl.Bool,
    unsat :: F.Formula a -> Pl.Bool
}

{- Todo:
    are there some sensible defaults for valid, unsat and sat,
    that hold in all possible semantics? (not only PC, LPC, ...)
    example for PC:
    -- negation of a tautology yields a contradiction
    valid = F.Not unsat
    -- negation of a contradiciton yields a tautology
    -- -> so its a satisfiable formula
    unsat = sat . F.Not
    valid = unsat . F.Not
    sat = to be implemented
-}


