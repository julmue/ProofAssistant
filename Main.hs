{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE ExistentialQuantification #-}

import System.Environment (getArgs)

import Text.Parsec (parse)

import Formula hiding (True,False)
import ParserProp
import ParserRequestArgs
import Prop
import qualified PropSemanticsPC as PC
import qualified PropSemanticsK3 as K3
import qualified PropSemanticsL3 as L3
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
        (NFAction _)            -> error "not yet defined"
        (HelpAction)            -> SB "help"

{- formula cassifications -}

getClassification :: SemanticsReq -> String -> Either String Property
getClassification sem s =
    case sem of
    PCReq -> classificationPC s
    L3Req -> classificationL3 s
    K3Req -> classificationK3 s
    LPReq -> classificationLP s
    RMReq -> classificationRM s

makeClassification :: Semantics Prop b -> String -> Either String Property
makeClassification sem s =
    case parse formulaProp "" s of
    (Left err) -> Left $ "Classification Propositional Calculus:" ++ show err
    (Right f) -> Right $ if sat sem f
                         then if valid sem f
                              then Valid
                              else Sat
                         else Unsat

classificationPC :: String -> Either String Property
classificationPC = makeClassification PC.semantics

classificationK3 :: String -> Either String Property
classificationK3 = makeClassification K3.semantics

classificationL3 :: String -> Either String Property
classificationL3 = makeClassification L3.semantics

classificationLP :: String -> Either String Property
classificationLP = undefined

classificationRM :: String -> Either String Property
classificationRM = undefined


{- property tasks -}
getProperty :: SemanticsReq -> String -> PropertyReq -> Either [Char] Bool
getProperty sem s pa =
    case sem of
    PCReq -> propertyPC s pa
    L3Req -> propertyL3 s pa
    K3Req -> propertyK3 s pa
    LPReq -> propertyLP s pa
    RMReq -> propertyRM s pa

makeProperty :: Semantics Prop b -> String -> PropertyReq -> Either [Char] Bool
makeProperty sem s pa =
    case parse formulaProp "" s of
    (Left err) -> Left $ "Property Propositional Calculus:" ++ show err
    (Right f) -> Right $ case pa of
        ValidReq -> valid sem f
        SatReq -> sat sem f
        UnsatReq -> unsat sem f

propertyPC :: String -> PropertyReq -> Either [Char] Bool
propertyPC = makeProperty PC.semantics

propertyK3 :: String -> PropertyReq -> Either [Char] Bool
propertyK3 = makeProperty K3.semantics

propertyL3 :: String -> PropertyReq -> Either [Char] Bool
propertyL3 = makeProperty L3.semantics

propertyLP :: String -> PropertyReq -> Either [Char] Bool
propertyLP = undefined

propertyRM :: String -> PropertyReq -> Either [Char] Bool
propertyRM = undefined


{- model task -}
getModels :: SemanticsReq -> String -> Either [Char] String
getModels sem s =
    case parse formulaProp "" s of
        (Left err) -> Left $ "Model Propositional Calculus:" ++ show err
        (Right f) -> Right $ case sem of
            PCReq -> show $ showModelsPC f
            K3Req -> show $ showModelsK3 f
            L3Req -> show $ showModelsL3 f
            LPReq -> undefined
            RMReq -> undefined

showModelsPC :: Formula Prop -> [[(Prop, PC.V)]]
showModelsPC = makeShowModels $ models PC.semantics

showModelsK3 :: Formula Prop -> [[(Prop, K3.V)]]
showModelsK3 = makeShowModels $ models K3.semantics

showModelsL3 :: Formula Prop -> [[(Prop, L3.V)]]
showModelsL3 = makeShowModels $ models L3.semantics

-- showModelsLP = undefined
-- showModelsRM = undefined




{- -}
-- normalform = undefined
-- help = undefined


{- ToDo:
    Languages of Formulas are not properly linked to the semantics ...
    as of now it is possible to attempt an evaluation of a LPC formula
    under PC semantics -- which throws.

    It should be possible to derive the set of languages a formula belongs to
    e.g.:   > derive "a && b": pc, lpc, l3, s4, ...
            > derive "Forall.a: Px": lpc, ...
    and return sensible error messages according to this classification.
-}
