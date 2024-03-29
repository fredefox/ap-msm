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
aState :: State
aState = State {
		prog = [], -- Not used for testing individual instructions.
		pc = 0,
		stack = [1,0],
		regs = Map.fromList [(0,42)]
	}

runInstruction :: State -> Inst -> Either Error State
runInstruction s instr =
	let (MSM f) = do _ <- interpInst instr; get
	in fmap snd $ f s

runInstruction' :: Inst -> Either Error State
runInstruction' = runInstruction aState

t00 :: Test
t00 = TestCase
	$ assertBool "Test instruction `PUSH`"
	$ Right aState { stack = [42,1,0], pc = 1 } == runInstruction' ( PUSH 42 )

t01 :: Test
t01 = TestCase
	$ assertBool "Test instruction `POP`"
	$ Right aState { stack = [0], pc = 1 } == runInstruction' POP

t02 :: Test
t02 = TestCase
	$ assertBool "Test instruction `DUP`"
	$ Right aState { stack = [1,1,0], pc = 1 } == runInstruction' DUP

t03 :: Test
t03 = TestCase
	$ assertBool "Test instruction `SWAP`"
	$ Right aState { stack = [0,1], pc = 1 } == runInstruction' SWAP

t04 :: Test
t04 = TestCase
	$ assertBool "Test instruction `NEWREG`"
	$ Right aState { regs = Map.fromList [(0,42),(42, 0)], pc = 1 } == runInstruction' ( NEWREG 42 )

t05 :: Test
t05 = TestCase
	$ assertBool "Test instruction `LOAD`"
	$ Right s { stack = [1337], pc = 1 } ==
		runInstruction s LOAD where
			s = State {
				prog = [],
				pc = 0,
				stack = [42],
				regs = Map.fromList [(42,1337)]
			}

t06 :: Test
t06 = TestCase
	$ assertBool "Test instruction `STORE`"
	$ Right aState { stack = [], regs = Map.fromList [(0,1)], pc = 1 } == runInstruction' STORE

t07 :: Test
t07 = TestCase
	$ assertBool "Test instruction `NEG`"
	$ Right aState { stack = [-1,0], pc = 1 } == runInstruction' NEG

t08 :: Test
t08 = TestCase
	$ assertBool "Test instruction `ADD`"
	$ Right aState { stack = [1], pc = 1 } == runInstruction' ADD

t09 :: Test
t09 = TestCase
	$ assertBool "Test instruction `JMP`"
	$ Right s { stack = [], pc = 42 }  == runInstruction s JMP where
		s = State { prog = [], pc = 0, stack = [42], regs = Map.empty }

t10 :: Test
t10 = TestCase
	$ assertBool "Test instruction `CJMP` with positive head of stack"
	$ Right s { stack = [], pc = 1 } == runInstruction s ( CJMP 1337 ) where
		s = State { prog = [], pc = 0, stack = [42], regs = Map.empty }

t11 :: Test
t11 = TestCase
	$ assertBool "Test instruction `CJMP` with negative head of stack"
	$ Right s { stack = [], pc = 1337 } == runInstruction s ( CJMP 1337 ) where
		s = State { prog = [], pc = 0, stack = [-42], regs = Map.empty }

t12 :: Test
t12 = TestCase
	$ assertBool "Test instruction `HALT`"
	$ Right aState == runInstruction' HALT

tests :: Test
tests = TestList [
		TestLabel "Individual instructions" $ TestList [
			t00, t01, t02, t03, t04,
			t05, t06, t07, t08, t09,
			t10, t11, t12
		],
		TestLabel "Sample program" $ TestList [
			TestCase
				$ assertBool "Test Program 42"
				$ let p42 = [
						NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH
						2, PUSH 0, LOAD, ADD, HALT
					]
				in Right 42 == runMSM p42
				]
	]

main :: IO Counts
main = runTestTT tests
