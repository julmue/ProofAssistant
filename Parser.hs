module Parser where 

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import qualified Formula as F

-- parameterization of the lexer
lexerParameters :: Token.LanguageDef ()
lexerParameters = Token.LanguageDef
    {   Token.commentStart      = "/*"
    ,   Token.commentEnd        = "*/"
    ,   Token.commentLine       = "//"
    ,   Token.nestedComments    = False
    ,   Token.identStart        = letter
    ,   Token.identLetter       = alphaNum <|> oneOf "_'"
    ,   Token.opStart           = Token.opLetter lexerParameters
    ,   Token.opLetter          = oneOf "`~!@$%^&+-*/=;:<>.?"
    ,   Token.reservedOpNames   = []
    ,   Token.reservedNames     = ["false","true","not","and","or","imp","iff","forall","exists"]
    ,   Token.caseSensitive     = True
    }    

-- construction of the lexer using the lexerStyle parameter-collection
-- () means no user definede state
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser lexerParameters

-- bind token parsers to names
identifier :: Parser String
identifier = Token.identifier lexer

-- example:
--  parse identifier "(unknown)" "hello" 
