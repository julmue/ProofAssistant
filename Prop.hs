module Prop where

import PrettyPrint

newtype Prop = Prop { propName :: String } deriving (Eq, Ord)

instance Show Prop where
    show = propName

instance PrettyPrint Prop where
    prettyPrint = propName

