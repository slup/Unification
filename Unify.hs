module Unify (Term(Var, Cst, Fun), MGU(Failure, List),
              Substitution(Subst), unify) where

-- Definitions of Term, Substitution, and MGU
import Definitions

-- Unification algorithm of two Terms.
-- The result of a unification is a list of Substitutions or a Failure
-- MGU collects the various Substitutions.

-- Deals with Failure and two equal terms
unify :: Term -> Term -> MGU -> MGU
unify x y theta = Failure


{--
  Unify(x, y, theta) returns a substitution to make x and y identical
    input x: a Variable, Constant, List, or Compound
          y: a Variable, Constant, List, or Compound
          theta: the substitutions build up so far (optional, defaults empty)
    if theta = failure then return failure
    else if x = y then return theta
    else if isVariable(x) then return UnifyVar(x, y, theta)
    else if isVariable(y) then return UnifyVar(y, x, theta)
    else if isCompound(x) and isCompound(y) then
       return Unify(Args[x], Args[y], Unify(OP[x], OP[y], theta))
    else if isList(x) and isList(y) then
      return Unify(Rest[x], Rest[y], Unify(First[x], First[y], theta))
    else return failure
--}

-- *** Add you code here
