module Semantics
    (Semantics (..)
    )
where

import Formula as F
import Prelude as Pl

-- class of semantics for a

class Semantics s where
    isModel :: s (F.Formula a) -> model -> F.Formula a -> Pl.Bool
    models :: s (F.Formula a) -> F.Formula a -> [model]
    valid :: s (F.Formula a) -> F.Formula a -> Pl.Bool
    sat :: s (F.Formula a) -> F.Formula a -> Pl.Bool
    unsat :: s (F.Formula a) -> F.Formula a -> Pl.Bool

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


