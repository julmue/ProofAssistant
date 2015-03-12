module Prop where

import PrettyPrint

newtype Prop = Prop { propName :: String } deriving (Show, Eq, Ord)

instance PrettyPrint Prop where
    prettyPrint = propName
     
