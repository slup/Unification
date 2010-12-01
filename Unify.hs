module Unify (Term(Var, Cst, Fun), MGU(Failure, List),
              Substitution(Subst), unify) where

-- Definitions of Term, Substitution, and MGU
import Definitions

-- Unification algorithm of two Terms.
-- The result of a unification is a list of Substitutions or a Failure
-- MGU collects the various Substitutions.

-- Deals with Failure and two equal terms
unify :: Term -> Term -> MGU -> MGU

-- *** Add you code here