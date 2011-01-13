{--
 - Werner Schwarz <schww1@bfh.ch>
 - Florian Bühlmann <buhlf4@bfh.ch>
 -}

import Test.HUnit
import Unify

test0 = TestCase $ assertEqual
	"{Y/apple, X/kiwi}"
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

test2 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(John, Jane)) = {X/Jane}"
	(List [Subst (Var 'X') (Cst "kiwi"),Subst (Var 'Y') (Cst "apple")])
	(unify
		(Fun "food" [Fun "fruit" [Cst "Limette", Var 'X'], Var 'X'])
		(Fun "food" [Fun "fruit" [Var 'X', Var 'Y'], Cst "Rohrzucker"])
		$ List []
	)

test3 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(Y, Bill)) = {X/Bill, Y/John}"
	(List [Subst (Var 'X') (Cst "kiwi"),Subst (Var 'Y') (Cst "apple")])
	(unify
		(Fun "food" [Fun "fruit" [Cst "Limette", Var 'X'], Var 'X'])
		(Fun "food" [Fun "fruit" [Var 'X', Var 'Y'], Cst "Rohrzucker"])
		$ List []
	)

test4 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(Y, Mother(Y))) = {Y/John, X/Mother(John)}"
	Failure
	(unify
		(Fun "food" [Fun "fruit" [Cst "Limette", Var 'X'], Var 'X'])
		(Fun "food" [Fun "fruit" [Var 'X', Var 'Y'], Cst "Rohrzucker"])
		$ List []
	)

test5 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(X, Elizabeth)) = fail"
	Failure
	(unify
		(Fun "food" [Fun "fruit" [Cst "Limette", Var 'X'], Var 'X'])
		(Fun "food" [Fun "fruit" [Var 'X', Var 'Y'], Cst "Rohrzucker"])
		$ List []
	)

test6 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(Z, Elizabeth)) = {X/Elizabeth, Z/John}"
	Failure
	(unify
		(Fun "food" [Fun "fruit" [Cst "Limette", Var 'X'], Var 'X'])
		(Fun "food" [Fun "fruit" [Var 'X', Var 'Y'], Cst "Rohrzucker"])
		$ List []
	)

main = do
	runTestTT $ TestList [test0, test1] --, test2, test3, test4, test5]

