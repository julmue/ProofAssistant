module ParserRequestArgs
    ( requestArgs
    )
where

import Control.Applicative

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
                (_, Left _) -> (s0, Right [])
                (s2, Right as) -> (s2, Right $ a:as)


type ParserArgs a = Parser [String] a

satisfy :: (s -> Bool) -> Parser [s] s
satisfy f = Parser $
    \s0 -> case s0 of
        [] -> (s0, Left "Error: end of stream")
        (s:s1) -> if f s
                  then (s1, Right s)
                  else (s0, Left "Error: did not satisfy")
