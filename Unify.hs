{--
 - Werner Schwarz <schww1@bfh.ch>
 - Florian Bühlmann <buhlf4@bfh.ch>
 -}

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
unify (Fun x xs) (Fun y ys) theta = unifyargslist xs ys $ unify (Cst x) (Cst y) theta -- unify xs ys ( unify x y theta )
unify _ _ _ = Failure

unifyargslist :: [Term] -> [Term] -> MGU -> MGU
unifyargslist [] [] theta = theta
unifyargslist (x:xs) (y:ys) theta = unifyargslist xs ys $ unify x y theta 
unifyargslist _ _ _ = Failure

--               var       x     theta
unifyvariable :: Term -> Term -> MGU -> MGU
unifyvariable _ _ Failure = Failure
unifyvariable var@(Var _) x@(Fun f args) theta@(List subs) = List (subs ++ [(Subst var (Fun f (substitutefunctionargs args theta)))])
unifyvariable var@(Var _) x theta@(List subs)
 | [] /= getsubstitutionlistcontaining var theta = unify (extractsecondterm (head (getsubstitutionlistcontaining var theta))) x theta
 | [] /= getsubstitutionlistcontaining x theta = unify var (extractsecondterm (head (getsubstitutionlistcontaining x theta))) theta
 | occurcheck var x = Failure
 | otherwise = List (subs ++ [(Subst var x)])

getsubstitutionlistcontaining :: Term -> MGU -> [Substitution]
--getsubstitutionlistcontaining var@(Fun f args) (List subs) = [x | x <- subs, y <- args, ((\(Subst term1 term2) -> term1) x) == y]
--getsubstitutionlistcontaining var@(Fun f args) (List subs) = [x | x <- subs, ((\(Subst term1 term2) -> term1) x) == var]
getsubstitutionlistcontaining var (List subs) = [x | x <- subs, ((\(Subst term1 term2) -> term1) x) == var]

extractsecondterm :: Substitution -> Term
extractsecondterm (Subst term1 term2) = term2

occurcheck :: Term -> Term -> Bool
occurcheck var@(Var _) (Fun function (firstarg:args)) = firstarg == var || occurcheck var (Fun function args)
occurcheck _ _ = False

substitutefunctionargs :: [Term] -> MGU ->  [Term]
substitutefunctionargs args (List subs) = [(extractsecondterm x) | x <- subs, y <- args, ((\(Subst term1 term2) -> term1) x) == y]

--(\x (List subs) = [x | x <- subs, x == term1])
--(\(Subst term1 term2) -> term2)

{--
      UnifyVar(var, x, theta) returns a substitution
        input var: a Variable,
              x: any expression
              theta: the substitutions build up so far
        if {var/val} in theta then return Unify(val, x, theta)
        else if {x/val} in theta then return Unify(var, val, theta)
        else if OccurCheck(var, x) then return failure
        else return add {var/x} to theta 
--}

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

    In a Compound expression, such as F(A, B), the function OP picks out the function symbol and the function Args picks out the argument list (A,B). First takes the first element of an argument list and Rest takes the argument list without the first element.
--}

-- *** Add you code here
