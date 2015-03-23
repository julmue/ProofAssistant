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
    let s = getTaskFormula t
        sem = getTaskSemantics t
    in  case getTaskAction t of
        ClassifyAction          -> SB $ getClassification sem s
        (PropertyAction pa)     -> SB $ getProperty sem s pa
        (ModelAction)           -> SB $ getModels sem s
--         (R.NFAction nf)       -> SB $ normalform f s nf
        (HelpAction)        -> SB $ "help"

{- formula cassifications -}

getClassification :: SemanticsReq -> String -> Either String Property
getClassification sem s =
    case sem of
    PCReq -> classificationPC s
    L3Req -> classificationL3 s
    K3Req -> classificationK3 s
    LPReq -> classificationLP s
    RMReq -> classificationRM s

makeClassification :: Semantics Prop b -> String -> Either [Char] Property
makeClassification sem s =
    case parse formulaProp "" s of
    (Left err) -> Left $ "Classification Propositional Calculus:" ++ show err
    (Right f) -> Right $ if sat sem f
                         then if valid sem f
                              then Valid
                              else Sat
                         else Unsat

classificationPC = makeClassification pc
classificationK3 = makeClassification k3
classificationL3 = undefined
classificationLP = undefined
classificationRM = undefined


{- property tasks -}
getProperty sem s pa =
    case sem of
    PCReq -> propertyPC s pa
    L3Req -> propertyL3 s pa
    K3Req -> propertyK3 s pa
    LPReq -> propertyLP s pa
    RMReq -> propertyRM s pa

makeProperty sem s pa =
    case parse formulaProp "" s of
    (Left err) -> Left $ "Property Propositional Calculus:" ++ show err
    (Right f) -> Right $ case pa of
        ValidReq -> valid sem f
        SatReq -> sat sem f
        UnsatReq -> unsat sem f

propertyPC = makeProperty pc
propertyK3 = makeProperty k3
propertyL3 = undefined
propertyLP = undefined
propertyRM = undefined


{- model task -}
-- getModels :: SemanticsReq -> String -> Either [Char] [[(Prop, PropSemanticsPC.V)]]
getModels sem s =
    case parse formulaProp "" s of
        (Left err) -> Left $ "Model Propositional Calculus:" ++ show err
        (Right f) -> Right $ case sem of
            PCReq -> show $ showModelsPC f
            K3Req -> show $ showModelsK3 f
            -- L3Req -> show $ showModelsL3 f
            -- LPReq -> show $ showModelsLP f
            -- RMReq -> show $ showModelsRM f

showModelsPC = makeShowModels $ models pc
showModelsK3 = makeShowModels $ models k3
showModelsL3 = undefined
showModelsLP = undefined
showModelsRM = undefined




{- -}
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
