{--
 - Werner Schwarz <schww1@bfh.ch>
 - Florian Bühlmann <buhlf4@bfh.ch>
 -}

import Test.HUnit
import Unify

test0 = TestCase $ assertEqual
	"{Y/Apple, X/Kiwi}"
	(List [Subst (Var 'X') (Cst "kiwi"),Subst (Var 'Y') (Cst "apple")])
	(unify 
		(Fun "food" [Var 'X', Fun "fruit" [Cst "apple", Var 'X']]) 
		(Fun "food" [Cst "kiwi", Fun "fruit" [Var 'Y', Var 'X']]) 
		$ List []
	)

test1 = TestCase $ assertEqual
	"Failure"
	Failure
	(unify
		(Fun "food" [Fun "fruit" [Cst "Limette", Var 'X'], Var 'X'])
		(Fun "food" [Fun "fruit" [Var 'X', Var 'Y'], Cst "Rohrzucker"])
		$ List []
	)

main = do
	runTestTT $ TestList [test0, test1] --, test2, test3, test4, test5]

