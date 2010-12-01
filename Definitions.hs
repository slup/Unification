-- Definitions of data structures produced by the parser and
-- used by the unification algorithm

module Definitions(Term(Var, Cst, Fun), Substitution(Subst), MGU(Failure, List)) where

-- A term is a variable, a constant, or a function with a list of arguments
data Term = Var Char | Cst String | Fun String [Term]  deriving (Eq, Show)

-- A substitution is composed of a variable and a term
data Substitution = Subst Term Term  deriving (Eq, Show)

-- Most General Unifier
data MGU = Failure | List [Substitution] deriving (Eq, Show)
