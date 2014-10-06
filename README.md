# Advanced Programming Assigmnet 2

Written by Frederik Hanghøj Iversen
for the course Advanced Programming
at Copenhagen University 2014

This report is for the second assignment in the course Advanced Programming.

This docuement is written in [GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown). It may best be viewed in the markdown-format but is also included as a pdf. The pdf-version was created with [pandoc](http://johnmacfarlane.net/pandoc/).

The project is maintened with git and repository acces can be granted by finding [me on GitHub](https://github.com/fredefox).

In this report I present the results of my efforts to implement a stack machine written in Haskell. The implementation is acheived through the use of a monadic type `MSM`. [The specification](http://www.diku.dk/~kflarsen/ap-2014/notes/msm.html) for this project is available from the course homepage for Advanced Programming. For the assigment a [skeletal implementation for the implementation](http://www.diku.dk/~kflarsen/ap-2014/notes/msm-skel.hs) was given. The structure in this project loosely follows this.

Source-code is attached along with pdf and markdown-versions of this report.

# Introduction
The code presented herein presents the implementation of a stack machine (`MSM`). The machine supports the following instructions:

  * PUSH
  * POP
  * DUP
  * SWAP
  * NEWREG
  * LOAD
  * STORE
  * NEG
  * ADD
  * JMP
  * CJMP
  * HALT

All code have been compiled without warnings using `-Wall`, nor are any hints given using `hlint`.

Please refer to the source-code when looking at the samples included in this report for completeness.

## Code structure
All source-code is available in `MSM.hs`.

All functions are annotated with comments where appropriate.

Each bullet in the specification corresponds to a comment in the source-code that start with:

```haskell
{-
 - ***
 -
 - ...
 -
 -}
```

### Tests
Unit-tests for the module is presented in `Test.hs`. There are two test-suites. One for each individual machine-instruction and one with an execution of a sample program.

## Code-review
Multiple definitions in this library are taken directly from the specification. These include: `Inst`, `Prog`, `Error`, `ErrorType`, and `runMSM`. Other definitions are given partially. One exception where I had to change the definition of a method that was given was `interp`. The specification was in contradiction with the provided skeletal code. I therefore change the definition to execute an extra `pop`-operation at the end of execution.

A `State` is defined like this:

```haskell
data State =
    State {
        prog  :: Prog,
        pc    :: Int,
        stack :: Stack,
        regs  :: Regs
    } deriving (Show)

initial :: Prog -> State
initial pr = State pr 0 [] Map.empty
```

Here the definition is given along with the convenience-function `initial` that creates the initial-state for an `MSM` with the program `pr`.


### The monad `MSM`
I initially defined the Micro Stack Machine like this:

```haskell
newtype MSM a = MSM (State -> (a, State))
```

My reason for chosing this came from looking at the `State`-monad as defined in the standard Haskell library. However, I much later found out - after making my initial implentation - that this definition was incompatible with the specification. I therefore had to refactor all my code to comply with the following definition.

```haskell
newtype MSM a = MSM (State -> Either Error (State, a))
```

An `MSM` is a `Monad` which in turn is a `Functor`. Here is how `MSM` becomes an instance of these:

```haskell
instance Functor MSM where
    f `fmap` MSM sfc = MSM $ \s -> case sfc s of
                                   (Left e) -> Left e
                                   (Right (s', a)) -> Right (s', f a)

instance Monad MSM where
    return a = MSM $ \s -> Right (s, a)
    (MSM sfc0) >>= f = MSM $ \s -> either Left nextStep $ sfc0 s
                        where nextStep (s', a) = let (MSM sfc1) = f a in sfc1 s'
```

I think maybe I could have reused the `fmap`-operator in my definition of `>>=` but I am not sure.

### Monadic machine-instructions

The machine instructions are:

  * `PUSH Int`
  * `POP`
  * `DUP`
  * `SWAP`
  * `NEWREG Int`
  * `LOAD`
  * `NEG`
  * `ADD`
  * `JMP`
  * `CJMP Int`
  * `HALT`


The implementation of the monadic instructions were rather tedious and quite straight-forward. An example of one such instructions is the `push`-function defined like this:


```haskell
push     :: Int -> MSM ()
push a   =  MSM $ \state ->
    Right (state { stack = a : stack state }, () )
```

The important thing to note about all these instructions are that they should be monadic - i.e. return an instance of `MSM`. I have chosen to let most of them return an `MSM ()` for instructions that were just supposed to alter the state of the machine.

### The interpreter
Working with the implementation I was doing detective-work to try and get the return-types to line up properly. It still confuses me a bit how `interp` can be a constant yet serve as a function that interprets something. I know this problem is at the heart of understanding monads - but it feels like I am a lot closer to understanding it.

My function `getInst` serves as a monadic way of accessing the next instruction to execute on the machine.

`interpInst` is basically just a method to translate from instances of `Inst` to functions that can be executed on an `MSM`.

The implementation of `interp` has been changed to make the specification congruent.

I had problems implementing the interpreter because I initially chose the wrong signature for my `MSM`-monad.

After this refactorization I had a problem trying to execute the sample program `p42` (found in the skeletal implementation). It was difficult for me to find the error but after it was brought to my attention that I could use `Debug.Trace` the error became apparant. The program counter was never incremented. Along with some minor adjustments my code finally worked.

### Testing
Different methods have been proposed for how to test the implementation. Here I will present my choice of testing along with viable alternatives along with argumentation why I think the approach I have chosen is most favorable.

3 ways of testing the code has been considered:

  * Unit-testing
  * Testing randomly generated programs
  * Testing a program equivalent to some computation and testing the the computation yields the same result with the machine as compared to testing it with some haskell-function
  * Optional task: Compiling to the `MSM`-language

#### Unit testing
In the first case we have unit-testing. This is the approach I have chosen. There is one test-case for each machine-instruction (except for CJMP where two test-cases were needed). This method was chosen and test-cases deliberately designed to give some form of coverage over all the machine-instructions. One thing that I should note here is that I have only implemented positive test-cases - that is, tests that show that a given computation works. I have not defined any test-cases that show that some error occur (like `pop`ping from an empty stack). My code contains two `TODO`-notes. (`MSM.hs:238`, `MSM.hs:247`)

I know that my program does at least not take these two cases into consideration:

  * A value is loaded from a non-allocated register.
  * A store is performed on a non-allocated register.

In the first case a runtime-error will occur, in the latter it it will succeed even though the specification calls for an error here.

Please refer to the source code for the full test-suite.

#### Random program
Generating random programs is not a good idea because no method can be implemented to show what the machine should return - nor if it should ever even halt.

#### Random input for pre-defined programs.
This method is a nice idea. I have chosen not do it alone for the reason that I felt that generating such a program would also only test a certain subset of "valid" programs thereby not offering many advantages over unit-testing. It is my belief that the same kind of coverage can be gained trough carefully constructed test-cases.

#### Compiling programs
This method has many of the same traits as above - of course with the added nicety of having a function able to compile to this machine that I have just implemented.

# Conclusion
In this report my efforts to succesfully implement a stack machine have been presented. Successful testing has been applied to the implementation although it has, at the same time, been highlighted that some slight adjustments is needed to increase robustness.
