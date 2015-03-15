module ParserRequest

where

import Request

-- import Text.Parsec
import Text.Parsec
import Text.Parsec.Char (noneOf)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

{- grammar for request object:

    request :=


-}

{- Ausgangspunkt:

    ein string:
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

    ReqExpr =
        Empty
        SemExpr ReqExpr
        FormExpr ReqExpr
        PQueryExpr ReqExpr
        NFormExpr ReqExpr
        HelpExpr ReqExpr

    SemExpr =
        Empty
        PC SemExpr
        L3 SenExpr

    PQueryExpr =
        ...

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
sem = pc <|> l3

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
    where delimiterFormula = "\'"

{- Parser for requests -}

-- tree
data ArgExpr
    = SemExpr [Semantics]
    | FormExpr String
    deriving Show

data LinearTree a
    = Node a (LinearTree a)
    | Leaf a
    deriving Show

linearTree :: Parser a -> Parser (LinearTree a)
linearTree pa =
    try (node pa) <|> (leaf pa)

node :: Parser a -> Parser (LinearTree a)
node pa =
    pa >>= \a ->
    (linearTree pa) >>= \t ->
    return $ Node a t

leaf :: Parser a -> Parser (LinearTree a)
leaf pa =
    pa >>= \a ->
    return $ Leaf a

argExpr :: Parser ArgExpr
argExpr=
        try (sems >>= \s -> return $ SemExpr s)
    <|> (formula >>= \f -> return $ FormExpr f)

argExprTree :: Parser (LinearTree ArgExpr)
argExprTree = linearTree argExpr
