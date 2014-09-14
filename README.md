# Advanced Programming Assigmnet 2

Written by Frederik Hanghøj Iversen
for the course Advanced Programming
at Copenhagen University 2014

This report is for the second assignment in the course Advanced Programming.

This docuement is written in [GitHub Flavored Markdown](https://help.github.com/articles/github-flavored-markdown). It may best be viewed in the markdown-format but is also included as a pdf. The pdf-version was created with [pandoc](http://johnmacfarlane.net/pandoc/).

The project is maintened with git and repository acces can be granted by finding [me on GitHub](https://github.com/fredefox). The hash of the current version that I am hereby submitting is `57d3dc66f3a1fd2c9b1c69eaf5be2bce60931c2c`.

In this report I present the results of my efforts to implement a stack machine written in Haskell. The implementation is acheived through the use of a monadic type `MSM`. [The specification](http://www.diku.dk/~kflarsen/ap-2014/notes/msm.html) for this project is available from the course homepage for Advanced Programming. For the assigment a [skeletal implementation for the implementation](http://www.diku.dk/~kflarsen/ap-2014/notes/msm-skel.hs) was given. The structure in this project loosely follows this.

Source-code is attached along with pdf and markdown-versions of this report.

# Introduction

## A note about non-completion
I commenced writing this report 45 minutes before deadline. I have been working all weekend on the project yet unfortunately I have not been able finish the project entirely.

I have implemented all the functions from the specification with the exception of `runMSM`. I could not get it to type-check.

Obviously this is not a full implementation but I hope that I will get an opportunity to resubmit my solution once I get some much needed feedback on my current attempt.

Since I do not have a full implementation it has also not been possible to do any testing on the current implementation.

My goal with this report will then be to document what I have been able to implement and evaluate what things I believe to have implemented correctly 

## Code structure
All source-code is available in `MSM.hs`.

All functions are annoted with comments where appropriate.

This is the first code I have ever written in Haskell. Perhaps my code illustrates this. My own thinknig is that the main tell probably is that I use a limited set of concepts from the programming language.

Each bullet in the specification corresponds to a comment in the source-code that start with:

```haskell
{-
 - ***
 -
 - ...
 -
 -}
```

## Code-review
Multiple definitions in this library are taken directly from the specification. These include: `Inst`, `Prog`, `Error`, `ErrorType`, `interp` and `runMSM`. Other definitions are given partially. No place in my code have I gone against a definition that was given.

I have not used `Error` or `ErrorType` anywhere in my code.

I will skip all the more or less trivial types and focus on the monads. Now. On to the things that I *did* define. A `State` is defined like this:

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
The Micro Stack Machine (`MSM`) itself is defined like this:

```haskell
newtype MSM a = MSM (State -> (a, State))
```

An `MSM` is `Monad` which in turn is a `Functor`. Here is how `MSM` becomes an instance of these:

```haskell
instance Functor MSM where
    f `fmap` MSM sfc = MSM $ \s -> let (a, b) = sfc s
                                   in (f a, b)

instance Monad MSM where
    return a = MSM $ const (a, initial [])
    (MSM sfc) >>= f = MSM $ \s -> let (a, b)  = sfc s
                                      (MSM g) = f a
                                  in   g b
```

I think maybe I could have reused the `fmap`-operator in my definition of `>>=` but I am not sure.

My inspiration for this implemention comes from the `State`-monad. My thinking is that an `MSM` is basically just a specific kind of `State`-monad where the "state" is unsurpisingly an instance of `State`.

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


```
push     :: Int -> MSM ()
push a   =  MSM $ \state ->
    ((), state { stack = a : stack state } )
```

The important thing to note about all these instructions are that they should be monadic - i.e. return an instance of `MSM`. I have chosen to let most of them return an `MSM ()` for instructions that were just supposed to alter the state of the machine.

### The interpreter
Implementing the interpreter was where I had the most serious problems and most likely the only place where my code has serious short-comings.

Working with the implementation I was doing detective-work to try and get the return-types to line up properly. It still confuses me a bit how `interp` can be a constant yet serve as a function that interprets something. I know this problem is at the heart of understanding monads - but it feels like I am a lot closer to understanding it.

My function `getInst` serves as a monadic way of accessing the next instruction to execute on the machine.

`interpInst` is basically just a method to translate from instances of `Inst` to functions that can be executed on an `MSM`.

The implementation of `interp` is taken directly from the specification.

`runMSM` does not typecheck and is left in the library for reference. Here is the implementation:

```haskell
runMSM :: Prog -> Either Error Int
runMSM p =
  let (MSM f) = interp
  in fmap snd $ f $ initial p
```

Here is the analysis that I performed on the issue (this is a quote from a question I posted on the course forum):

> `initial p` returns `State`, `f` has type `State -> ((), State)`. But I cant `fmap` `snd` in there on the second element in the tuple (which is what I recon it is trying to do since: `fmap snd ((),(1,2)) == ((),(2))`). But `State` obviously cannot be treated as a tuple.

### Testing
Unfortunately no formal testing has been done.

# Conclusion
My efforts to implement a monadic stack machine in haskell has been presented. The attempt was not a success. My own thought as to what I have done wrong has been presented. Work will continue on implementing this. And hopefully with some proper feedback a final solution will be presented.
