-- {-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module      : Semantics.Internal
-- Copyright   : (c) Julian Müller, 2013
-- License     : GPL-3
--
-- Maintainer  : jul.mue@hotmail.de
-- Stability   : experimental
-- Portability : POSIX
--
-- External interface of the "Logic.Semantics.Prop".
--

module Logic.Semantics.Prop
    (
    -- * Types
      Semantics (..)
    , TrVals (..)
    , Property(..)
    -- * Constructor functions
    , makeTrVals
    , makeSemantics
    -- * Prototype functions
    , protoDomain
    , protoModels
    , protoModelsLookup
    )
where

import Logic.Semantics.Prop.Internal
