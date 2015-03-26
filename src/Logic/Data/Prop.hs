{-# OPTIONS_GHC -Wall -Werror #-}

module Logic.Data.Prop
    ( Prop(..)
    ) where

import Clank.Data.PrettyPrint (PrettyPrint, prettyPrint)

newtype Prop = Prop { propName :: String } deriving (Eq, Ord)

instance Show Prop where
    show = propName

instance PrettyPrint Prop where
    prettyPrint = propName

