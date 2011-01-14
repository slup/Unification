{--
 - Werner Schwarz <schww1@bfh.ch>
 - Florian Bühlmann <buhlf4@bfh.ch>
 -}

-- Module that allows a user to enter two terms.
-- The two terms are parsed and unified if they are correct.
-- The result (most general unifier) is then displayed.

module UnifyMain(main) where

import UnifyParser
import Unify
import Data.Maybe

extractTerm :: (Term, [Char]) -> Term
extractTerm (a, b) = a


-- Main function
main :: IO()
main =	do
			putStr "Term 1: "
			t1 <- getLine
			putStr "Term 2: "
			t2 <- getLine
			
			let x = extractTerm(fromJust(term t1))
			let y = extractTerm(fromJust(term t2))
			
			let mgu = unify x y (List [])

			-- for plain List output: putStrLn (show (mgu))
			putStrLn (show (mgu))
			--putStrLn ("MGU   : {" ++ formatoutputstring (getsubstitutionlist mgu))
			
getsubstitutionlist :: MGU -> [Substitution]
getsubstitutionlist (List subs) = [x | x <- subs] --, ((\(Subst term1 term2) -> Subst term1 term2) x)]
getsubstitutionlist _ = []

formatoutputstring :: [Substitution] -> String
formatoutputstring ((Subst term1 term2):[]) = substitutionformat term1 term2 ++ "}"  
formatoutputstring ((Subst term1 term2):xs) = substitutionformat term1 term2 ++ ", " ++ formatoutputstring xs
formatoutputstring _ = "(FormatFailure)"

substitutionformat :: Term -> Term -> String
substitutionformat term1@(Var var) term2@(Cst cst) = [var] ++ "/" ++ cst
--substitutionformat term1@(Fun funcname1 [args1]) term2@(Fun funcname2 [args2]) = [var] ++ "/" ++ cst -- replace with singletermformat
--substitutionformat term1@(Fun fun) term2 = [var] ++ "/" ++ cst -- replace with singletermformat
--substitutionformat term1 term2@(Fun fun) = [var] ++ "/" ++ cst -- replace with singletermformat
substitutionformat _ _ = "(SubstitutionFailure or nested Function)"

{--
ideas for better/complete output formating
singletermformat :: Term -> String
singletermformat term1@(Var var) = [var]
singletermformat term1@(Cst cst) = cst
singletermformat term1@(Fun funcname1 arg:args) = funcname1 ++ "(" ++ singletermformat arg ++ (multipletermsformat args) ++ ")"

multipletermsformat :: [Term] -> String
multipletermsformat [] = ""
multipletermsformat x@(Var var):[] = [var]
multipletermsformat x@(Var var):xs = [var] ++ "," ++ multipletermsformat xs
multipletermsformat x@(Cst cst):[] = cst
multipletermsformat x@(Cst cst):xs = cst ++ "," ++ multipletermsformat xs
multipletermsformat x@(Fun funcname1 arg:args):[] = singletermformat arg ++ multipletermsformat args
multipletermsformat x@(Fun funcname1 arg:args):xs = singletermformat arg ++ multipletermsformat args ++ multipletermsformat xs
--}