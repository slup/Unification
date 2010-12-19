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
			putStrLn "Please enter term one:"
			t1 <- getLine
			putStrLn "Please enter term two:"
			t2 <- getLine
			
			let x = extractTerm(fromJust(term t1))
			let y = extractTerm(fromJust(term t2))
			
			putStrLn (show(unify x y (List [])))
			
