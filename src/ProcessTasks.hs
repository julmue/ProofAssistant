module ProcessTasks (
    processTasks
    ) where

import Text.Parsec (parse)

import Formula hiding (True,False)
import ParserProp
import ParserRequestArgs
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
processTasks tasks = fmap processTask tasks

processTask :: Task -> ShowBox
processTask t =
    let s = getTaskFormula t
        sem = getTaskSemantics t
    in  case getTaskAction t of
        TurnstileAction ss      -> ShowBox $ getTurnstile sem s ss
        ClassifyAction          -> ShowBox $ getClassification sem s
        (PropertyAction pa)     -> ShowBox $ getProperty sem s pa
        (ModelAction)           -> ShowBox $ getModels sem s
        (NFAction _)            -> error "not yet defined"
        (HelpAction)            -> ShowBox "help"

{- entailing relation -}
getTurnstile :: SemanticsReq -> String -> [String] -> Either String Bool
getTurnstile sem s ss =
    case sem of
    PCReq -> turnstilePC s ss
    L3Req -> turnstileL3 s ss
    K3Req -> turnstileK3 s ss
    LPReq -> turnstileLP s ss
    RMReq -> turnstileRM s ss

makeTurnstyle sem s ss =
    case parse formulaProp "" s of
    (Left err) -> Left $ show err
    (Right f) -> case ss of
        [] -> Right $ valid sem f
        _ -> case sequence $ fmap (parse formulaProp "") ss of
            (Left err) -> Left $ show err
            (Right fs) -> undefined

turnstilePC = undefined
turnstileK3 = undefined
turnstileL3 = undefined
turnstileLP = undefined
turnstileRM = undefined

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
propertyLP = makeProperty LP.semantics

propertyRM :: String -> PropertyReq -> Either [Char] Bool
propertyRM = makeProperty LP.semantics


{- model task -}
getModels :: SemanticsReq -> String -> Either [Char] String
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
showModelsPC = makeShowModels $ models PC.semantics

showModelsK3 :: Formula Prop -> [[(Prop, K3.V)]]
showModelsK3 = makeShowModels $ models K3.semantics

showModelsL3 :: Formula Prop -> [[(Prop, L3.V)]]
showModelsL3 = makeShowModels $ models L3.semantics

showModelsLP :: Formula Prop -> [[(Prop, LP.V)]]
showModelsLP = makeShowModels $ models LP.semantics

showModelsRM :: Formula Prop -> [[(Prop, RM.V)]]
showModelsRM = makeShowModels $ models RM.semantics

