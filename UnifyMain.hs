-- Module that allows a user to enter two terms.
-- The two terms are parsed and unified if they are correct.
-- The result (most general unifier) is then displayed.

module UnifyMain(main) where

import UnifyParser
import Unify


-- Main function
main :: IO()
main =	do
			putStrLn "Please enter term one:"
			x <- getLine
			putStrLn "Please enter term two:"
			y <- getLine
			
			putStrLn (unify (term x) (term y))
			
