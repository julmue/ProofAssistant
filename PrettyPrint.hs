module PrettyPrint (PrettyPrint (..)) where

class PrettyPrint a where 
    prettyPrint :: a -> String	
