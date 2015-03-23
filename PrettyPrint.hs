{-# OPTIONS_GHC -Wall -Werror #-}

module PrettyPrint (PrettyPrint (..)) where

class Show a =>  PrettyPrint a where
    prettyPrint :: a -> String
    prettyPrint = show
