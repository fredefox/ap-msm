{-
 - ***
 -
 - This module implements all the functions of the Micro Stack Machine (or MSM for short)-library.
 -
 - The implementation follows the specification located at:
 -
 -     [advanced programming course homepage](http://www.diku.dk/~kflarsen/ap-2014/notes/msm.html)
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
module MSM where

{-
 - ***
 -
 - Decleration of an instruction (`Inst`) and a program (`Prog`)
 -
 -}
data Inst
    = PUSH Int
    | POP
    | DUP
    | SWAP
    | NEWREG Int
    | LOAD
    | NEG
    | ADD
    | JMP
    | CJMP Int
    | HALT
    deriving (Eq,Show)

type Prog = [Inst]

{-
 - ***
 -
 - Declare a stack (`Stack`), registers (`Regs`) and the state of the MSM (`State`)
 -
 -}
type Stack = [Int]

type Regs = [Int]

data State =
	State {
		prog  :: Prog,
		pc    :: Int,
		stack :: Stack,
		regs  :: Regs
	}

{-
 - ***
 -
 - Declare error types (`ErrorType`) and errors (`Error`)
 -
 -}

data ErrorType =
	| StackUnderflow
	| UnallocatedRegister Int
	| RegisterAlreadyAllocated
	| InvalidPc
	| Unspec String

data Error = Error { errorType :: ErrorType }
