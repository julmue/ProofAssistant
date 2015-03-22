module ParserRequestArgs
    ( Error,
      toTasks
    )
where

import Control.Applicative
import Control.Monad

import Request


requestArgs = undefined

{-  Simple backtracking Parser to operate on a stream of strings;
    for parsing the return value of getArgs.
    Maybe this can be rewritten as a ParsecT parser ...
-}

type Error = String

newtype Parser s a = Parser { runParser :: s -> (s, Either Error a) }


instance Functor (Parser s) where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser pa) = Parser $
        \s0 -> case pa s0 of
            (_, Left err) -> (s0, Left err)
            (s1, Right a) -> (s1, Right $ f a)


instance Applicative (Parser s) where
    -- pure :: a -> (Parser s) a
    pure a = Parser $
        \s0 -> (s0, Right a)
    -- (apply) <*> :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pfab) <*> (Parser pa) = Parser $
        \s0 -> case pfab s0 of
            (_, Left err) -> (s0, Left err)
            (s1, Right fab) -> case pa s1 of
                (_, Left err) -> (s0, Left err)
                (s2, Right a) -> (s2, Right $ fab a)


instance Alternative (Parser s) where
    -- empty :: (Parser s) a
    empty = Parser $ \s0 -> (s0, Left "empty")
    -- (choice/alternative) (<|>) :: Parser a -> Parser a -> Parser a
    (Parser pa1) <|> (Parser pa2) = Parser $
        \s0 -> case pa1 s0 of
            resp1@(_, Right _) -> resp1
            (_, Left _) -> case pa2 s0 of
                resp2@(_, Right _) -> resp2
                errp2@(_, Left err) -> (s0, Left err)
    -- some: 1 or more
    -- some :: (Parser s) a -> (Parser s) [a]
    some p@(Parser pa) = Parser $
        \s0 -> case pa s0 of
            (_, Left err) -> (s0, Left err)
            (s1, Right a) -> case (runParser $ many p) s1 of
                (_, Left err) -> (s1, Right [a])
                (s2, Right as) -> (s2, Right $ a:as)
    -- many: 0 or more
    -- many :: (Parser s) a -> (Parser s) [a]
    many p@(Parser pa) = Parser $
        \s0 -> case pa s0 of
            (_, Left _) -> (s0, Right [])
            (s1, Right a) -> case (runParser $ some p) s1 of
                (_, Left _) -> (s1, Right [a])
                (s2, Right as) -> (s2, Right $ a:as)

instance Monad (Parser s) where
    -- return :: a -> (Parser s) a
    return = pure
    -- (bind) (>>=) :: (Parser s) a -> (a -> (Parser s) b) -> Parser b
    (Parser pa) >>= fab = Parser $
        \s0 -> case pa s0 of
            (_, Left err) -> (s0, Left err)
            (s1, Right a) -> runParser (fab a) s1


satisfy :: (s -> Bool) -> Parser [s] s
satisfy f = Parser $
    \s0 -> case s0 of
        [] -> (s0, Left "Error: end of stream")
        (s:s1) -> if f s
                  then (s1, Right s)
                  else (s0, Left "Error: did not satisfy")

type ParserTok a = Parser [String] a

oneOf :: [String] -> ParserTok String
oneOf sl = satisfy (`elem` sl)

noneOf :: [String] -> ParserTok String
noneOf sl = satisfy $ not . (flip elem) sl


flagFormula = ["-f","--formula"]
flagSemantics = ["-s","--semantics"]
flagClassification = ["-c","--classify"]
flagProperty = ["-p","--properties"]
flagModel = ["-m","-ms","--models","--model"]
flagNormalForm = ["-n","--normalform"]
flagHelp = ["-h","--help"]

flags = flagFormula ++ flagSemantics ++ flagClassification ++ flagProperty ++
        flagModel ++ flagNormalForm ++ flagHelp

-- scanOptions
-- scans a stream of argument tokens for flag and flag-options until the next flag
-- can also be used to scan for switches: return (Left err) if a flag is not found
-- there should be a way to distinguish between flags with not arguments (switches
-- and flags that take arguments (Options)
scanOptions :: [String] -> [String] -> Parser [String] [String]
scanOptions flags flag =
    ((++) <$> options flags flag <*> scanOptions flags flag)
    <|> options flags flag
    where options flags flag = many (noneOf flag) *> oneOf flag *> (many (noneOf flags))


scanWithFlags       = scanOptions flags

-- scanners for the flags
-- switches return Left err on failure
scanFormulas        = scanWithFlags flagFormula
scanSemantics       = scanWithFlags flagSemantics
scanClassification  = scanWithFlags flagClassification
scanProperties      = scanWithFlags flagProperty
scanModels          = scanWithFlags flagModel
scanNormalForms     = scanWithFlags flagNormalForm
scanHelp            = scanWithFlags flagHelp

toSemantics :: String -> Either Error Semantics
toSemantics s = case s of
    "pc"    -> Right PC
    "l3"    -> Right L3
    x       -> Left $ "Error(toSemantics): " ++ x ++ " is not a known semantics!"

toProp :: String -> Either Error Prop
toProp s = case s of
    "valid" -> Right Valid
    "sat"   -> Right Sat
    "unsat" -> Right Unsat
    x       -> Left $ "Error(toProp): " ++ x ++ " is not a known porperty!"

toNormalForms :: String -> Either Error NormalForm
toNormalForms s = case s of
    "cnf"   -> Right CNF
    "dnf"   -> Right DNF
    x       -> Left $ "Error(toNormalForm): " ++ x ++ " is not a known normal form!"

-- Maybe this step should be split in two ...

getFormulas :: [String] -> Either Error [String]
getFormulas s = case getArgs scanFormulas s of
    (Left _)        -> Left $ "Error(getFormulas): No formulas specified"
    (Right [])      -> Left $ "Error(getFormulas): No formulas specified"
    (Right fs)      -> (Right fs)

getSemantics :: [String] -> Either Error [Semantics]
getSemantics s = case getArgs scanSemantics s of
    (Left _)        -> Left $ "Error(getSemantics): No semantics specified"
    (Right [])      -> Left $ "Error(getSemantics): No semantics specified"
    (Right ss)      -> sequence $ fmap toSemantics ss

getClassification :: [String] -> Either Error Bool
getClassification s = case getArgs scanClassification s of
    (Left _)        -> Right False
    (Right [])      -> Right True
    (Right x)       -> Left $ "Error(getClassification): unknown Argument(s): " ++ (concat $ fmap show x)

getProps :: [String] -> Either Error [Prop]
getProps s = case getArgs scanProperties s of
    (Left _)        -> Right []
    (Right x)       -> sequence $ fmap toProp x

getModels :: [String] -> Either Error Bool
getModels s = case getArgs scanModels s of
    (Left _)        -> Right False
    (Right [])      -> Right True
    (Right x)       -> Left $ "Error(getModels): unknown Argument(s): " ++ (concat $ fmap show x)

getNormalForms :: [String] -> Either Error [NormalForm]
getNormalForms s = case getArgs scanNormalForms s of
    (Left _)        -> Right []
    (Right x)       -> sequence $ fmap toNormalForms x

getHelp :: [String] -> Either Error Bool
getHelp s = case getArgs scanHelp s of
    (Left _)        -> Right False
    (Right [])      -> Right True
    (Right _)       -> Right True

getArgs :: Parser s a -> s -> Either Error a
getArgs p s = snd $ runParser p s


type Args = [String]


requestConstructor :: Args -> Either Error Request
requestConstructor args =
    pure Request
    <*> getFormulas args
    <*> getSemantics args
    <*> getClassification args
    <*> getProps args
    <*> getModels args
    <*> getNormalForms args
    <*> getHelp args


tasksConstructor :: Request -> [Task]
tasksConstructor req =
    let fs = getReqFormulas req
        ss = getReqSemantics req
        ps = getReqProps req
        nfs = getReqNormalForms req
    in  [ Task f s ClassifyAction   | f <- fs, s <- ss, getReqClassify req] ++
        [ Task f s (PropAction p)   | f <- fs, s <- ss, p <- ps ] ++
        [ Task f s ModelAction      | f <- fs, s <- ss, getReqModels req ] ++
        [ Task f s (NFAction nf)    | f <- fs, s <- ss, nf <- nfs ] ++
        [ Task [] PC HelpAction     | (getReqHelp req) ]

-- toTasks :: Args -> [Task]
toTasks args = case tasksConstructor <$> requestConstructor args of
    err@(Left _)    -> err
    (Right [])      -> Right $ [ Task [] PC HelpAction ]
    x               -> x
