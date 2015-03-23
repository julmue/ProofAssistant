{-# LANGUAGE ExistentialQuantification #-}

module ShowBox (
    ShowBox (ShowBox)
    ) where

data ShowBox = forall s. Show s => ShowBox s

instance Show ShowBox where
    show (ShowBox s) = show s

