module ParserRequestArgs
    ( requestArgs
    )
where

import Request

-- import Text.Parsec
import Text.Parsec
import Text.Parsec.Char (noneOf)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

{- situation:

    input: a string
    * "s: PC,L3 pq: classify, models nf: cnf,dnf f: a && b "
    * "pq: classify"
    * ""
    * "pq: models"
    * "nf: cnf s: PC"
    * ...

    every flag can be there but is optional
    if flag isn't issued with a ':' the default value should be applied
    order shouldn't matter

    so we need parsers that skip an amount of characters
    until they reach 'their' information (or not),
    then parse it...
    after all possible parsers have been applied all the information
    is collected and the request object can be constructed.

-}

-- the best would be to parse the formula just as a string
-- and to not apply the formula Parser yet ...

languageDef :: Token.LanguageDef ()
languageDef = Token.LanguageDef
    {   Token.commentStart      = ""
    ,   Token.commentEnd        = ""
    ,   Token.commentLine       = ""
    ,   Token.nestedComments    = False
    ,   Token.opStart           = Token.opLetter languageDef
    ,   Token.opLetter          = letter
    ,   Token.identStart        = letter
    ,   Token.identLetter       = alphaNum
    ,   Token.reservedNames     = ["-s","-f","-p","-n","pc","l3"]
    ,   Token.reservedOpNames   = Token.reservedNames languageDef
    ,   Token.caseSensitive     = False
    }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

-- extract the lexical parsers from 'lexer'
-- these parsers strip trainling whitespace after tokens
identifier  :: Parser String
identifier  = Token.identifier lexer
reserved    :: String -> Parser ()
reserved    = Token.reserved lexer
reservedOp  :: String -> Parser ()
reservedOp  = Token.reservedOp lexer
parens      :: Parser a -> Parser a
parens      = Token.parens lexer
integer     :: Parser Integer
integer     = Token.integer lexer
semi        :: Parser String
semi        = Token.semi lexer
whiteSpace  :: Parser ()
whiteSpace  = Token.whiteSpace lexer

{- parser for semantic arguments -}
sems :: Parser [Semantics]
sems = reserved "-s" >>
       many1 sem >>= \s ->
       return s

sem :: Parser Semantics
sem = try pc <|> l3

pc :: Parser Semantics
pc = reserved "pc" >>
     whiteSpace >>
     return PC

l3 :: Parser Semantics
l3 = reserved "l3" >>
     whiteSpace >>
     return L3

{- parser for formula (parses formula only as string) -}
formula :: Parser String
formula =
    reserved "-f" >>
    oneOf delimiterFormula  >>
    many1 (noneOf delimiterFormula ) >>= \f ->
    oneOf delimiterFormula  >>
    return f
    where delimiterFormula = "<<"

{- parser for classification queries -}
classify :: Parser Classify
classify =
    reserved "classify" >>
    whiteSpace >>
    return Classify


{- parser for property queries -}
valid :: Parser Prop
valid =
    reserved "valid" >>
    whiteSpace >>
    return Valid

sat :: Parser Prop
sat =
    reserved "sat" >>
    whiteSpace >>
    return Sat

unsat :: Parser Prop
unsat =
    reserved "unsat" >>
    whiteSpace >>
    return Unsat

prop :: Parser Prop
prop =  try sat
    <|> try unsat
    <|> valid

props :: Parser [Prop]
props =
    reserved "-p" >>
    whiteSpace >>
    many1 prop

{- parser for model queries -}
m :: Parser Model
m = reserved "model" >>
    whiteSpace >>
    return Model

ms :: Parser Model
ms =    reserved "models" >>
        whiteSpace >>
        return Models

model :: Parser Model
model = try ms
    <|> m

{- Parser for help request -}
help :: Parser Help
help =
    reserved "-h" >>
    whiteSpace >>
    return Help

{- Parser for normal forms -}
cnf :: Parser NormalForm
cnf =
    reserved "cnf" >>
    whiteSpace >>
    return CNF

dnf :: Parser NormalForm
dnf =
    reserved "dnf" >>
    whiteSpace >>
    return DNF

normalform :: Parser NormalForm
normalform =
    dnf <|> cnf

normalforms :: Parser [NormalForm]
normalforms =
    reserved "-n" >>
    whiteSpace >>
    many1 normalform

{- Parser for request arguments -}
argExpr :: Parser Arg
argExpr=
        try (formula >>= \f -> whiteSpace >> (return $ ArgFormula f))
    <|> try (sems >>= \ss -> whiteSpace >> (return $ ArgSemantics ss))
    <|> try (classify >> (return ArgClassify))
    <|> try (props >>= \qs -> whiteSpace >> (return $ ArgProps qs))
    <|> try (model >>= \ms -> whiteSpace >> (return $ ArgModel ms))
    <|> try (normalforms >>= \ns -> whiteSpace >> (return $ ArgNFs ns))
    <|> (help >> whiteSpace >> (return ArgHelp))

requestArgs :: Parser [Arg]
requestArgs = many1 argExpr

-- TODO:
-- get rid of all the 'whiteSpace' all over the code
-- shouldn't the tokenizer take care of that?
-- this is an awful lot of boilerplate ... reduce that!

