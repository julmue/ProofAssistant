-- {-# OPTIONS_GHC -Wall -Werror #-}

module Semantics.Semantics
    ( Semantics (..)
    , TrVals(..)
    , Property(..)
    , makeTrVals
    , makeDomain
    , makeSemantics
    , makeModels
    , makeModelsLookup
    )
where

import Semantics.SemanticsInternal
        ( Semantics (..)
    , TrVals(..)
    , Property(..)
    , makeTrVals
    , makeSemantics
    , makeDomain
    , makeModels
    , makeModelsLookup
    , protoSat
    , protoValid
    , protoEntails
    , intersectModelLookups
    , extendModel
    , sortModels
    , association
    )

