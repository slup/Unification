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
		(Fun "weather" [Fun "mix" [Cst "Rain", Var 'X'], Var 'X'])
		(Fun "weather" [Fun "mix" [Var 'X', Var 'Y'], Cst "Snow"])
		$ List []
	)

test2 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(John, Jane)) = {X/Jane}"
	(List [Subst (Var 'X') (Cst "Jane")])
	(unify
		(Fun "Knows" [Cst "John", Var 'X'])
		(Fun "Knows" [Cst "John", Cst "Jane"])
		$ List []
	)

test3 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(Y, Bill)) = {X/Bill, Y/John}"
	(List [Subst (Var 'Y') (Cst "John"), Subst (Var 'X') (Cst "Bill")])
	(unify
		(Fun "Knows" [Cst "John", Var 'X'])
		(Fun "Knows" [Var 'Y', Cst "Bill"])
		$ List []
	)

test4 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(Y, Mother(Y))) = {Y/John, X/Mother(John)}"
	(List [Subst (Var 'Y') (Cst "John"),Subst (Var 'X') (Fun "Mother" [(Cst "John")])])
	(unify
		(Fun "Knows" [Cst "John", Var 'X'])
		(Fun "Knows" [Var 'Y', (Fun "Mother" [Var 'Y'])])
		$ List []
	)

test5 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(X, Elizabeth)) = fail"
	Failure
	(unify
		(Fun "Knows" [Cst "John", Var 'X'])
		(Fun "Knows" [Var 'X', Cst "Elizabeth"])
		$ List []
	)

test6 = TestCase $ assertEqual
	"UNIFY(Knows(John, X), Knows(Z, Elizabeth)) = {X/Elizabeth, Z/John}"
	(List [Subst (Var 'Z') (Cst "John"), Subst (Var 'X') (Cst "Elizabeth")])
	(unify
		(Fun "Knows" [Cst "John", Var 'X'])
		(Fun "Knows" [Var 'Z', Cst "Elizabeth"])
		$ List []
	)

main = do
	runTestTT $ TestList [test0, test1, test2, test3, test4, test5, test6]

