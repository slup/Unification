-- Module that contains the basic parsing functions

module CoreParser(Parser, char, return, fail, (#), (!), (?), (#>), (>->),
                  Parse, parse, toString, fromString) where

import Prelude hiding (return, fail)
infixl 3 !
infixl 7 ?
infixl 6 #
infixl 5 >->
infixl 4 #>

class Parse a where
    parse :: Parser a
    fromString :: String -> a
    fromString cs =
        case parse cs of
               Just(s, []) -> s
               Just(s, cs) -> error ("garbage '"++cs++"'")
               Nothing -> error "Nothing"
    toString :: a -> String

type Parser a = String -> Maybe (a, String)

char :: Parser Char
char []= Nothing
char (c:cs) = Just (c, cs)

return :: a -> Parser a
return a cs = Just (a, cs)

fail ::  Parser a
fail cs = Nothing

(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
             Nothing -> n cs
             mcs -> mcs

(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs =
    case m cs of
    Nothing -> Nothing
    Just(r, s) -> if p r then Just(r, s) else Nothing

(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs =
    case m cs of
    Nothing -> Nothing
    Just(a, cs') ->
        case n cs' of
        Nothing -> Nothing
        Just(b, cs'') -> Just((a, b), cs'')

(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> b) cs =
    case m cs of
    Just(a, cs') -> Just(b a, cs')
    Nothing -> Nothing

(#>) :: Parser a -> (a -> Parser b) -> Parser b
(p #> k) cs =
    case p cs of
    Nothing -> Nothing
    Just(a, cs') -> k a cs'
