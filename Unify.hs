module Unify (Term(Var, Cst, Fun), MGU(Failure, List),
              Substitution(Subst), unify) where

-- Definitions of Term, Substitution, and MGU
import Definitions

-- Unification algorithm of two Terms.
-- The result of a unification is a list of Substitutions or a Failure
-- MGU collects the various Substitutions.

-- Deals with Failure and two equal terms
unify :: Term -> Term -> MGU -> MGU
unify _ _ Failure = Failure
unify x y theta
  | x == y = theta
unify x@(Var _) y theta = unifyvariable x y theta
unify x y@(Var _) theta = unifyvariable y x theta
unify (Fun x xs) (Fun y ys) theta = unifylist xs ys $ unify (Cst x) (Cst y) theta -- unify xs ys ( unify x y theta )
--unify (x:xs) (y:ys) theta = unify xs ys $ unify x y theta -- unify xs ys ( unify x y theta )
unify _ _ _ = Failure

unifylist :: [Term] -> [Term] -> MGU -> MGU
unifylist [] [] theta = theta
unifylist (x:xs) (y:ys) theta = unifylist xs ys $ unify x y theta 
unifylist _ _ _ = Failure

--               var       x     theta
unifyvariable :: Term -> Term -> MGU -> MGU
unifyvariable _ _ Failure = Failure
unifyvariable var@(Var _) x theta@(List subs) =

getvariablefromsubstitutionlist :: Term -> MGU -> MGU
getvariablefromsubstitutionlist var (List subs) = (List [x | x <- subs, ((\(Subst term1 term2) -> term1) x) == var])

extractsecondterm :: Substitution -> Term
extractsecondterm (Subst term1 term2) = term2

--(\x (List subs) = [x | x <- subs, x == term1])
--(\(Subst term1 term2) -> term2)


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
