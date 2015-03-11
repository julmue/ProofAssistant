module Parser where 

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Expr as Expr

import Data.Functor.Identity (Identity)

import qualified Formula as F

{- Lexer -}

-- parameterization of the lexer
languageDef :: Token.LanguageDef ()
languageDef = Token.LanguageDef
    {   Token.commentStart      = "/*"
    ,   Token.commentEnd        = "*/"
    ,   Token.commentLine       = "//"
    ,   Token.nestedComments    = False
    ,   Token.identStart        = letter
    ,   Token.identLetter       = alphaNum <|> oneOf "_'"
    ,   Token.opStart           = Token.opLetter languageDef
    ,   Token.opLetter          = oneOf "`~!@$%^&+-*/=;:<>.?"
    ,   Token.reservedOpNames   = [".","~","&&", "||","->","<->"]
    ,   Token.reservedNames     = ["false","true","not","and","or","imp","iff","forall","exists"]
    ,   Token.caseSensitive     = True
    }    

-- construction of the lexer using the lexerStyle parameter-collection
-- () means no user definede state
-- lexer is a collection of lexival parsers
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

-- example:
-- parse identifier "(unknown)" "hello"
-- > right "hello"

{- main parser -}

true :: Parser (F.Formula a)
true = 
    reserved "true" >>
    return F.True
    
false :: Parser (F.Formula a)
false =
    reserved "false" >>
    return F.False

atom :: Parser a -> Parser (F.Formula a)
atom p =    
    p >>= \content ->
    return $ F.Atom content 
    
operators :: [[Expr.Operator String () Identity (F.Formula a)]]
operators = 
    [   [Expr.Prefix (reservedOp "~"        >> return (F.Not ))                                     ]
    ,   [Expr.Infix  (reservedOp "&&"       >> return (F.And ))                     Expr.AssocRight ]    
    ,   [Expr.Infix  (reservedOp "||"       >> return (F.Or  ))                     Expr.AssocRight ]  
    ,   [Expr.Infix  (reservedOp "->"       >> return (F.Imp ))                     Expr.AssocRight ]  
    ,   [Expr.Infix  (reservedOp "<->"      >> return (F.Iff ))                     Expr.AssocRight ] 
    ]

formula :: Parser a -> Parser (F.Formula a)
formula p = 
    Expr.buildExpressionParser operators 
    (    parens (formula p)
     <|> true
     <|> false 
     <|> atom p
    )

-- example: 
-- (parse $ formula letter) "" "atom h"
-- > Atom 'h'

