module Prop where

import PrettyPrint

newtype Prop = Prop { propName :: String } deriving (Show, Eq)

instance PrettyPrint Prop where
    prettyPrint p = propName p
    
