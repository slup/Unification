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
			putStrLn ("Raw Output: " ++ show (mgu))
			putStrLn ("MGU   : {" ++ formatoutputstring (getsubstitutionlist mgu))
			
getsubstitutionlist :: MGU -> [Substitution]
getsubstitutionlist (List subs) = [x | x <- subs] --, ((\(Subst term1 term2) -> Subst term1 term2) x)]
getsubstitutionlist _ = []

formatoutputstring :: [Substitution] -> String
formatoutputstring ((Subst term1 term2):[]) = substitutionformat term1 term2 ++ "}"  
formatoutputstring ((Subst term1 term2):xs) = substitutionformat term1 term2 ++ ", " ++ formatoutputstring xs
formatoutputstring _ = "(FormatOutputStringFailure)"

substitutionformat :: Term -> Term -> String
substitutionformat term1@(Var var) term2@(Cst cst) = [var] ++ "/" ++ cst
substitutionformat term1@(Var var) term2@(Fun f args) = [var] ++ "/" ++ f ++ "(" ++ (formatfunctionargs args) ++")"
substitutionformat _ _ = "(SubstitutionFormatFailure)"

formatfunctionargs :: [Term] -> String
formatfunctionargs (arg@(Var var):[]) = [var]
formatfunctionargs (arg@(Cst cst):[]) = cst
formatfunctionargs (arg@(Var var):args) = [var] ++ "," ++ (formatfunctionargs args)
formatfunctionargs (arg@(Cst cst):args) = cst ++ "," ++ (formatfunctionargs args)
