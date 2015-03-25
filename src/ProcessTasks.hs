{-# OPTIONS_GHC -Wall -Werror #-}

module ProcessTasks (
    processTasks
    ) where

import Text.Parsec (parse)

import Formula hiding (True,False)
import ParserProp
import Prop
import qualified PropSemanticsPC as PC
import qualified PropSemanticsK3 as K3
import qualified PropSemanticsL3 as L3
import qualified PropSemanticsLP as LP
import qualified PropSemanticsRM as RM
import Request
import Semantics
import ShowBox (ShowBox(..))

processTasks :: [Task] -> [ShowBox]
processTasks = fmap processTask

processTask :: Task -> ShowBox
processTask t =
    let s = getTaskFormula t
        sem = getTaskSemantics t
    in  case getTaskAction t of
        TurnstileAction ss      -> unpack $ getTurnstile sem s ss
        ClassifyAction          -> unpack $ getClassification sem s
        (PropertyAction pa)     -> unpack $ getProperty sem s pa
        (ModelAction)           -> unpack $ getModels sem s
        (NFAction _)            -> error "not yet defined"
        (HelpAction)            -> ShowBox "help"
  where unpack result = case result of
            (Left err) -> ShowBox err
            (Right value) -> ShowBox value

{- entailing relation -}
getTurnstile :: SemanticsReq -> String -> [String] -> Either String Bool
getTurnstile sem s ss =
    case sem of
    PCReq -> turnstile PC.semantics s ss
    L3Req -> turnstile L3.semantics s ss
    K3Req -> turnstile K3.semantics s ss
    LPReq -> turnstile LP.semantics s ss
    RMReq -> turnstile RM.semantics s ss

turnstile :: Semantics Prop b -> String -> [String] -> Either String Bool
turnstile sem s ss =
    case parse formulaProp "" s of
    (Left err) -> Left $ show err
    (Right f) -> case ss of
        [] -> Right $ valid sem f
        _ -> case sequence $ fmap (parse formulaProp "") ss of
            (Left err) -> Left $ show err
            (Right fs) -> Right $ entails sem fs f

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
classificationLP = makeClassification LP.semantics

classificationRM :: String -> Either String Property
classificationRM = makeClassification RM.semantics


{- property tasks -}
getProperty :: SemanticsReq -> String -> PropertyReq -> Either String Bool
getProperty sem s pa =
    case sem of
    PCReq -> propertyPC s pa
    L3Req -> propertyL3 s pa
    K3Req -> propertyK3 s pa
    LPReq -> propertyLP s pa
    RMReq -> propertyRM s pa

makeProperty :: Semantics Prop b -> String -> PropertyReq -> Either String Bool
makeProperty sem s pa =
    case parse formulaProp "" s of
    (Left err) -> Left $ "Property Propositional Calculus:" ++ show err
    (Right f) -> Right $ case pa of
        ValidReq -> valid sem f
        SatReq -> sat sem f
        UnsatReq -> unsat sem f

propertyPC :: String -> PropertyReq -> Either String Bool
propertyPC = makeProperty PC.semantics

propertyK3 :: String -> PropertyReq -> Either String Bool
propertyK3 = makeProperty K3.semantics

propertyL3 :: String -> PropertyReq -> Either String Bool
propertyL3 = makeProperty L3.semantics

propertyLP :: String -> PropertyReq -> Either String Bool
propertyLP = makeProperty LP.semantics

propertyRM :: String -> PropertyReq -> Either String Bool
propertyRM = makeProperty LP.semantics


{- model task -}
getModels :: SemanticsReq -> String -> Either String String
getModels sem s =
    case parse formulaProp "" s of
        (Left err) -> Left $ "Model Propositional Calculus:" ++ show err
        (Right f) -> Right $ case sem of
            PCReq -> show $ showModelsPC f
            K3Req -> show $ showModelsK3 f
            L3Req -> show $ showModelsL3 f
            LPReq -> show $ showModelsLP f
            RMReq -> show $ showModelsRM f

showModelsPC :: Formula Prop -> [[(Prop, PC.V)]]
showModelsPC = modelsLookup PC.semantics

showModelsK3 :: Formula Prop -> [[(Prop, K3.V)]]
showModelsK3 = modelsLookup K3.semantics

showModelsL3 :: Formula Prop -> [[(Prop, L3.V)]]
showModelsL3 = modelsLookup L3.semantics

showModelsLP :: Formula Prop -> [[(Prop, LP.V)]]
showModelsLP = modelsLookup LP.semantics

showModelsRM :: Formula Prop -> [[(Prop, RM.V)]]
showModelsRM = modelsLookup RM.semantics

