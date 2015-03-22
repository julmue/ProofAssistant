{-# OPTIONS_GHC -XExistentialQuantification #-}

-- {-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Control.Monad
import Data.List (unwords)
import System.Environment (getArgs)

import qualified Formula as F
import FormulaPCLaws
import Parser
import ParserFormula
import ParserProp
import ParserRequestArgs
import PrettyPrint
import qualified Prop as P
import qualified PropSemanticsPC as PC
import qualified Request as R
import qualified Semantics as S

import Text.Parsec
import Text.Parsec.String

data Prop
    = Valid
    | Sat
    | Unsat
    deriving (Show, Eq)

main :: IO ()
-- main = toTasks <$> getArgs >>= print
main = do
    args <- getArgs
    case toTasks args of
        (Left err) -> print err
        (Right tasks) -> mapM_ print $ fmap processTask tasks


-- processTasks = fmap processTasks
data ShowBox = forall s. Show s => SB s

instance Show ShowBox where
    show (SB c) = show c

processTask :: R.Task -> ShowBox
processTask t =
    let f = R.getTaskFormula t
        s = R.getTaskSemantics t
    in  case R.getTaskAction t of
        R.ClassifyAction      -> SB $ classification s f
--         (R.PropAction pa)     -> SB $ property f s pa
--         (R.ModelAction ma)    -> SB $ models f s ma
--         (R.NFAction nf)       -> SB $ normalform f s nf
--         (R.HelpAction)        -> SB $ help

{- formula cassifications -}

classification :: R.Semantics -> String -> Either String Prop
classification sem s =
    case sem of
    R.PC -> classificationPC s
    R.L3 -> classificationL3 s

classificationPC :: String -> Either String Prop
classificationPC s =
    case parse formulaProp "" s of
    (Left err) -> Left $ "Classification Propositional Calculus:" ++ show err
    (Right f) -> Right $ if S.sat PC.pc f
                         then if S.valid PC.pc f
                              then Valid
                              else Sat
                         else Unsat

-- classificationL3 :: String -> Either String Prop
classificationL3 = undefined


{- -}
property = undefined
models = undefined
normalform = undefined
help = undefined


-- helper functions
parseCLI :: Parser a -> String -> Either ParseError a
parseCLI p = parse p "Command Line"


{- ToDo:
    Languages of Formulas are not properly linked to the semantics ...
    as of now it is possible to attempt an evaluation of a LPC formula
    under PC semantics -- which throws.

    It should be possible to derive the set of languages a formula belongs to
    e.g.:   > derive "a && b": pc, lpc, l3, s4, ...
            > derive "Forall.a: Px": lpc, ...
    and return sensible error messages according to this classification.
-}
