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

import Test.HUnit
import MSM
import qualified Data.Map as Map

{-
 - One test-case for each machine-instruction
 -}
t00 = TestCase
	$ assertBool "`PUSH`"
	$ undefined

t01 = TestCase
	$ assertBool "`POP`"
	$ undefined

t02 = TestCase
	$ assertBool "`DUP`"
	$ undefined

t03 = TestCase
	$ assertBool "`SWAP`"
	$ undefined

t04 = TestCase
	$ assertBool "`NEWREG`"
	$ undefined

t05 = TestCase
	$ assertBool "`LOAD`"
	$ undefined

t06 = TestCase
	$ assertBool "`STORE`"
	$ undefined

t07 = TestCase
	$ assertBool "`NEG`"
	$ undefined

t08 = TestCase
	$ assertBool "`ADD`"
	$ undefined

t09 = TestCase
	$ assertBool "`JMP`"
	$ undefined

t10 = TestCase
	$ assertBool "`CJMP`"
	$ undefined

t11 = TestCase
	$ assertBool "`HALT`"
	$ undefined

