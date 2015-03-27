{-# OPTIONS_GHC -Wall -Werror #-}

-- |
-- Module: Logic.Data.Formula
--
-- External interface of "Logic.Data.Prop".
--

module Logic.Data.Prop
    (
    -- * Types
    Prop(..)
    ) where

import Clank.Data.PrettyPrint (PrettyPrint, prettyPrint)

-- | Type of propositions
--
newtype Prop = Prop { propName :: String } deriving (Eq, Ord)

-- | 'Prop' instance of Show.
instance Show Prop where
    show = propName

-- | 'Prop' instance of PrettyPrint.
instance PrettyPrint Prop where
    prettyPrint = propName

