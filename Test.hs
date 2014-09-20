{-
 - ***
 -
 - This module tests the implementation of the Micro Stack Machine
 -
 - Written by Frederik Hanghøj Iversen
 - for the course Advanced Programming
 - at The University of Copenhagen 2014
 -
 - me@fredefox.eu /^._
 -  ,___,--~~~~--' /'~
 -  `~--~\ )___,)/'
 -      (/\\_  (/\\_
 -
 -}
module Test where

import Test.HUnit hiding (State)
import MSM
import qualified Data.Map as Map

{-
 - One test-case for each machine-instruction
 -}
-- This state is convenient for testing because all operations on this state is permitted.
aState = State {
		prog = [], -- Not used for testing individual instructions.
		pc = 0,
		stack = [1,0],
		regs = Map.fromList [(0,42)]
	}

runInstruction :: State -> Inst -> Either Error State
runInstruction s instr =
	let (MSM f) = do interpInst instr; get
	in fmap snd $ f $ s

runInstruction' = runInstruction aState

t00 = TestCase
	$ assertBool "`PUSH`"
	$ Right aState { stack = [42,1,0], pc = 1 } == runInstruction' ( PUSH 42 )

t01 = TestCase
	$ assertBool "`POP`"
	$ Right aState { stack = [0], pc = 1 } == runInstruction' POP

t02 = TestCase
	$ assertBool "`DUP`"
	$ Right aState { stack = [1,1,0], pc = 1 } == runInstruction' DUP

t03 = TestCase
	$ assertBool "`SWAP`"
	$ Right aState { stack = [0,1], pc = 1 } == runInstruction' SWAP

t04 = TestCase
	$ assertBool "`NEWREG`"
	$ Right aState { regs = Map.fromList [(0,42),(42, 0)], pc = 1 } == runInstruction' ( NEWREG 42 )

t05 = TestCase
	$ assertBool "`LOAD`"
	$ Right s { stack = [1337], pc = 1 } ==
		runInstruction s LOAD where
			s = State {
				prog = [],
				pc = 0,
				stack = [42],
				regs = Map.fromList [(42,1337)]
			}

t06 = TestCase
	$ assertBool "`STORE`"
	$ Right aState { stack = [], regs = Map.fromList [(0,1)], pc = 1 } == runInstruction' ( STORE )

t07 = TestCase
	$ assertBool "`NEG`"
	$ Right aState { stack = [-1,0], pc = 1 } == runInstruction' NEG

t08 = TestCase
	$ assertBool "`ADD`"
	$ Right aState { stack = [1], pc = 1 } == runInstruction' ADD

t09 = TestCase
	$ assertBool "`JMP`"
	$ Right s { stack = [], pc = 42 }  == runInstruction s JMP where
		s = State { prog = [], pc = 0, stack = [42], regs = Map.empty }

t10 = TestCase
	$ assertBool "`CJMP` - positive head of stack"
	$ Right s { stack = [], pc = 1 } == runInstruction s ( CJMP 1337 ) where
		s = State { prog = [], pc = 0, stack = [42], regs = Map.empty }

t11 = TestCase
	$ assertBool "`CJMP` - negative head of stack"
	$ Right s { stack = [], pc = 1337 } == runInstruction s ( CJMP 1337 ) where
		s = State { prog = [], pc = 0, stack = [-42], regs = Map.empty }

t12 = TestCase
	$ assertBool "`HALT`"
	$ Right aState == runInstruction' HALT

tests = TestList [
		TestLabel "Individual instructions" $ TestList [
			t00, t01, t02, t03, t04,
			t05, t06, t07, t08, t09,
			t10, t11, t12
		],
		TestLabel "Other possible test-suites" $ TestList [
		]
	]

main = runTestTT tests
