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
    | STORE
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
	} deriving (Show, Eq)

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
--newtype MSM a = MSM (State -> (a, State))
newtype MSM a = MSM (State -> Either Error (State, a))

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
	 -
	 - Jonas showed me his implementation of `fmap`:
	 -
	 -   `fmap = liftM`
	 -}
	f `fmap` MSM sfc = MSM $ \s -> case sfc s of
								   (Left e) -> Left e
								   (Right (s', a)) -> Right (s', f a)

instance Monad MSM where
	{-
	 - Packs a value in an `MSM`
	 -
	 - Type signature:
	 -
	 -   return :: a -> MSM a
	 -}
	return a = MSM $ \s -> Right (s, a)
	{-
	 - `sfc`   is a stateful computation.
	 - `f`     so far I'll just say that it's a function
	 -         - the current instruction perhaps?
	 -
	 - Type signature:
	 -
	 -   (>>=) :: MSM a -> (a -> MSM b) -> MSM b
	 -}
	(MSM sfc0) >>= f = MSM $ \s -> either Left nextStep $ sfc0 s
						where nextStep (s', a) = let (MSM sfc1) = f a in sfc1 s'

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
	Right (state { stack = a : stack state }, () )

pop      :: MSM Int
pop      =  MSM $ \state ->
	case stack state of
	[] -> Left $ Error StackUnderflow
	x:xs -> Right (state { stack = xs }, x )

incr     :: MSM ()
incr     =  MSM $ \s -> Right ( s { pc = succ $ pc s }, () )
-- These functions are derivable from previously derived monadic instructions
-- Jonas argues that they should live in `interpInst` - I like having them
-- defined here so the mapping from `Inst` to these functions are entirely
-- trivial.
dup      :: MSM ()
dup      =  do
	x <- pop
	push x
	push x

get      :: MSM State
get      = MSM $ \s -> Right (s,s)

set      :: State -> MSM ()
set s    = MSM $ const $ Right (s,())

modify   :: (State -> State) -> MSM ()
modify f =  MSM $ \s -> Right (f s, ())

swap     :: MSM ()
swap     =  do
	x <- pop
	y <- pop
	_ <- push x
	push y

newreg   :: Int -> MSM ()
newreg i =  MSM $ \state ->
	Right (state { regs = Map.insert i 0 $ regs state }, () )


load     :: MSM ()
load     =  MSM $ \state ->
	case stack state of
	[] -> Left $ Error StackUnderflow
	x:xs -> Right (state { stack = k:xs }, ()) where
		-- TODO: Runtime error here:
		k = regs state Map.! x

store    :: MSM ()
store    =  MSM $ \state ->
	case stack state of
	[] -> Left $ Error StackUnderflow
	[_] -> Left $ Error StackUnderflow
	v:i:xs -> Right ( state { regs = r, stack = xs}, ()) where
		-- TODO: Should actually throw an error if `i` haven't been allocated.
		r = Map.insert i v $ regs state

neg      :: MSM ()
neg      =  do
	x <- pop
	push (-x)

add      :: MSM ()
add      =  do
	x <- pop
	y <- pop
	push $ x + y

jmp      :: MSM ()
jmp      =  do
	x <- pop
	MSM $ \s -> Right (s { pc = x }, ())

cjmp     :: Int -> MSM ()
cjmp i   =  do
	x <- pop
	if x < 0 then
		MSM $ \s -> Right (s { pc = i }, ())
	else
		MSM $ \s -> Right (s { pc = pc s + 1 }, ())

halt     :: MSM Bool
halt     =  return False

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
getInst = MSM $ \state ->
	let
		p = prog state
		i = pc   state
	in
		if 0 <= i && i < length p
		then
			Right (state, p !! i)
		else
			Left $ Error InvalidPc

interpInst :: Inst -> MSM Bool
interpInst (PUSH i) = do push i; incr; return True
interpInst POP = do _ <- pop; incr; return True
interpInst DUP = do dup; incr; return True
interpInst SWAP = do swap; incr; return True
interpInst (NEWREG i) = do newreg i; incr; return True
interpInst LOAD = do load; incr; return True
interpInst STORE = do store; incr; return True
interpInst NEG = do neg; incr; return True
interpInst ADD = do add; incr; return True
interpInst JMP = do jmp; return True
interpInst (CJMP i) = do cjmp i; return True
interpInst HALT = return False

{-
 - I want something like this, but I can't:
 -
 -   interpInst inst = case inst of
 -     POP -> pop
 -     DUP -> dup
 -     return True
 -}
interp :: MSM Int
interp = do
		run
		pop
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
runMSM :: Prog -> Either Error Int
runMSM p =
	let (MSM f) = interp
	in fmap snd $ f $ initial p
