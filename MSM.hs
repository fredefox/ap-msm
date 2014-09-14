{-
 - ***
 -
 - This module implements all the functions of the Micro Stack Machine (or MSM
 - for short)-library.
 -
 - The implementation follows the specification located at:
 -
 -     [advanced programming course
 -     homepage](http://www.diku.dk/~kflarsen/ap-2014/notes/msm.html)
 -
 - The code structure is loosely based on the handed-out skeletal
 - implementation:
 -
 -     [msm-skel.hs](http://www.diku.dk/~kflarsen/ap-2014/notes/msm-skel.hs)
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
 - Imports
 -
 -}
import Control.Monad

import qualified Data.Map as Map

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
 - Declare a stack (`Stack`), registers (`Regs`) and the state of the MSM
 - (`State`)
 -
 -}
type Stack = [Int]

type Regs = Map.Map Int Int

data State =
	State {
		prog  :: Prog,
		pc    :: Int,
		stack :: Stack,
		regs  :: Regs
	} deriving (Show)

initial :: Prog -> State
initial pr = State pr 0 [] Map.empty
{-
 - ***
 -
 - Declare error types (`ErrorType`) and errors (`Error`)
 -
 -}

data ErrorType
	= StackUnderflow
	| UnallocatedRegister Int
	| RegisterAlreadyAllocated
	| InvalidPc
	| Unspec String
	deriving (Show, Read, Eq)

data Error =
	Error {
		errorType :: ErrorType
	} deriving (Show, Eq)

{-
 - ***
 -
 - Definition of the MSM (`MSM`)
 -
 -}
newtype MSM a = MSM (State -> (a, State))

{-
 - An `MSM` is a specific kind of state-monad where the current state (not
 - suprisingly) is an instance of a `State`. A state-monad should return the
 - new state along with the result of some computation. The result, then, in
 - some cases can be the head of the stack - or in other cases it can be the
 - empty type `()` for operation that do not produce a "result", like `push`.
 -
 - That is, given the state of the machine, this monad computes
 -
 -   `s0 -> (a, s1)`
 -
 - where
 -
 -   `a`  is the result of some computation
 -   `s0` is the state before the computation
 -   `s1` is the new state resulting from the single computational step
 -}
instance Functor MSM where
	{-
	 - (fmap) :: (a -> b) -> MSM a -> MSM b
	 -}
	f `fmap` MSM sfc = MSM $ \s -> let (a, b) = sfc s
								   in (f a, b)

instance Monad MSM where
	{-
	 - Packs a value in an `MSM`
	 -
	 - Type signature:
	 -
	 -   return :: a -> MSM a
	 -}
	return a = MSM $ const (a, initial [])
	{-
	 - `sfc`   is a stateful computation.
	 - `f`     so far I'll just say that it's a function
	 -         - the current instruction perhaps?
	 -
	 - Type signature:
	 -
	 -   (>>=) :: MSM a -> (a -> MSM b) -> MSM b
	 -}
	(MSM sfc) >>= f = MSM $ \s -> let (a, b)  = sfc s
	                                  (MSM g) = f a
                                  in   g b

{-
 - ***
 -
 - Decleration of monadic instructions corresponding to `Instr`:
 -
 -
 - * `PUSH Int`
 - * `POP`
 - * `DUP`
 - * `SWAP`
 - * `NEWREG Int`
 - * `LOAD`
 - * `NEG`
 - * `ADD`
 - * `JMP`
 - * `CJMP Int`
 - * `HALT`
 -
 - These functions must all have the type signature:
 -
 -   (MSM m) => a -> m b`
 -
 - For some types `a` and `b`
 -
 - Ponder: `Instr`'s constituent parts have capital names. Functions can only
 - be lower case. How does the connection between these two work?
 -
 -}
push     :: Int -> MSM ()
push a   =  MSM $ \state ->
	((), state { stack = a : stack state } )

pop      :: MSM Int
pop      =  MSM $ \state ->
	let x:xs = stack state
	in (x, state { stack = xs} )

dup      :: MSM ()
dup      =  MSM $ \state ->
	let xs@(x:_) = stack state
	in ((), state { stack = x:xs } )

swap     :: MSM ()
swap     =  MSM $ \state ->
	let x:x':xs = stack state
	in ((), state { stack = x':x:xs } )

newreg   :: Int -> MSM ()
newreg i =  MSM $ \state ->
	((), state { regs = Map.insert i 0 $ regs state } )

load     :: MSM ()
load     =  MSM $ \state ->
	let x:xs = stack state
	    k    = regs state Map.! x
	in ((), state { stack = k:xs } )

store    :: MSM ()
store    =  MSM $ \state ->
	let v:i:xs = stack state
	    r = Map.insert i v $ regs state
	in ((), state { regs = r, stack = xs } )

neg      :: MSM ()
neg      =  MSM $ \state ->
	let x:xs = stack state
	in ((), state { stack = (-x):xs } )

add      :: MSM ()
add      =  MSM $ \state ->
	let x:x':xs = stack state
	in ((), state { stack = (x+x'):xs} )

jmp      :: MSM ()
jmp      =  MSM $ \state ->
	let x:xs = stack state
	in ((), state { pc = x, stack = xs } )

cjmp     :: Int -> MSM ()
cjmp i   =  MSM $ \state ->
	let x:xs = stack state
	    pc' | x < 0 = i | otherwise = (pc state + 1)
	in ((), state { pc = pc', stack = xs } )

halt     :: MSM ()
halt     =  MSM $ \state -> ((), state)

{-
 - ***
 -
 - Implementation of `interp`
 -
 - This functions type definition confuses me a bit. It's similiar to how
 - io-functions have the type `IO ()`, that is, they dont really return
 - anything they just produce the result that stuff is printed to the screen.
 - This should work similiar I suppose, because the `state` of and `MSM`
 - already defines the program. So it should just take that machine and run
 - it's program.
 -
 - To implement `interp` a two helper-functions will be defined first:
 -
 -   * `getInst`
 -   * `interInst`
 -
 -}
getInst :: MSM Inst
getInst = MSM $ \state -> ( prog state !! pc state, state)

-- Small convenience-method for `interpInst`
shouldHalt :: MSM a -> MSM Bool
shouldHalt m = MSM $ \state -> (HALT == prog state !! pc state, state)

interpInst :: Inst -> MSM Bool
interpInst (PUSH i) = shouldHalt $ push i
interpInst POP = shouldHalt $ pop
interpInst DUP = shouldHalt $ dup
interpInst SWAP = shouldHalt $ swap
interpInst (NEWREG i) = shouldHalt $ newreg i
interpInst LOAD = shouldHalt $ load
interpInst NEG = shouldHalt $ neg
interpInst ADD = shouldHalt $ add
interpInst JMP = shouldHalt $ jmp
interpInst (CJMP i) = shouldHalt $ cjmp i
interpInst HALT = shouldHalt $ halt


interp :: MSM ()
interp = run
	where run = do
		inst <- getInst
		cont <- interpInst inst
		when cont run

{-
 - ***
 -
 - Implementation of `runMSM`
 -
 - This implementation is taken directly from the skeletal implementaion.
 -
 -}
-- This does not type-check and therefore it is outcommented.
--runMSM :: Prog -> Either Error Int
--runMSM p =
--	let (MSM f) = interp
--	in fmap snd $ f $ initial p
