{-# OPTIONS_GHC -XExistentialQuantification #-}

{-- -# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Control.Monad
import Data.List (unwords)
import System.Environment (getArgs)

import Text.Parsec
import Text.Parsec.String

import Formula hiding (True,False)
import FormulaPCLaws
import Parser
import ParserFormula
import ParserProp
import ParserRequestArgs
import PrettyPrint
import Prop
import PropSemanticsPC
import PropSemanticsK3
import Request
import Semantics

main :: IO ()
-- main = toTasks <$> getArgs >>= print
main = do
    args <- getArgs
    case toTasks args of
        (Left err) -> print err
        (Right tasks) -> mapM_ print $ fmap processTask tasks



-- some helper functions that should get sourced out to their own files at some point ...
data ShowBox = forall s. Show s => SB s

instance Show ShowBox where
    show (SB c) = show c

processTask :: Task -> ShowBox
processTask t =
    let f = getTaskFormula t
        s = getTaskSemantics t
    in  case getTaskAction t of
        ClassifyAction      -> SB $ classification s f
--         (R.PropertyAction pa)     -> SB $ property f s pa
--         (R.ModelAction ma)    -> SB $ models f s ma
--         (R.NFAction nf)       -> SB $ normalform f s nf
        (HelpAction)        -> SB $ "help"

{- formula cassifications -}

classification :: SemanticsReq -> String -> Either String Property
classification sem s =
    case sem of
    PCReq -> classificationPC s
    L3Req -> classificationL3 s
    K3Req -> classificationK3 s
    LPReq -> classificationLP s
    RMReq -> classificationRM s

classificationPC :: String -> Either String Property
classificationPC s =
    case parse formulaProp "" s of
    (Left err) -> Left $ "Classification Propositional Calculus:" ++ show err
    (Right f) -> Right $ if sat pc f
                         then if valid pc f
                              then Valid
                              else Sat
                         else Unsat

classificationL3 :: String -> Either String Property
classificationL3 = undefined

classificationK3 = undefined

classificationLP = undefined

classificationRM = undefined


{- -}
property = undefined
models = undefined
normalform = undefined
help = undefined


{- ToDo:
    Languages of Formulas are not properly linked to the semantics ...
    as of now it is possible to attempt an evaluation of a LPC formula
    under PC semantics -- which throws.

    It should be possible to derive the set of languages a formula belongs to
    e.g.:   > derive "a && b": pc, lpc, l3, s4, ...
            > derive "Forall.a: Px": lpc, ...
    and return sensible error messages according to this classification.
-}
