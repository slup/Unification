-- Parser that parses a term according to the following BNF grammar

-- term ::= cst | var | fun '(' term+ ')'

-- Variable names are composed of single uppercase letters
-- Constant and function names are words (letters) that are not variable names

-- Examples of terms: Fruit(Lemon, X)
--                    Tree(Tree(X, Couple (ctea, Y)), Z))

-- This module exports the parsing function 'term' that parses a term, which
-- returns a term of type Term as defined in the Definitions module.

module UnifyParser(term) where

import Parser
import Char

-- Definitions of data structures
import Definitions

cons(a, b) = a:b

-- Each of the following function parses a given element and produces
-- a element of type Term (see Definition module)

-- Constant
cst :: Parser Term
cst =  ((char ? isUpper # word >-> cons) !
        (char ? isLower # word >-> cons) !
        (char ? isLower >-> (\ c -> [c])))
       >-> (\ s -> Cst s)

-- Variable
var :: Parser Term
var =  (token (char ? isUpper)) >-> (\ c -> Var c)

-- Argument list
args :: Parser [Term]
args =  iter (term #- lit ',') # term
        >-> (\ x -> fst x ++ [snd x])

-- Function
fun :: Parser Term
fun =  word #- lit '(' # args #- lit ')'
       >-> (\ (w,as) -> Fun w as )

-- Term
term :: Parser Term
term =  fun ! cst ! var