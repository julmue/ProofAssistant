{-# OPTIONS_GHC -Wall -Werror #-}

module Logic.Parser.Prop
    ( prop
    , formulaProp
    ) where

import Control.Applicative ((<$>))

import Text.Parsec.Char	(letter)
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1)

import Logic.Parser.Formula
import Logic.Data.Prop
import Logic.Data.Formula

prop :: Parser Prop
prop =  Prop <$> many1 letter

formulaProp :: Parser (Formula Prop)
formulaProp = formula prop
