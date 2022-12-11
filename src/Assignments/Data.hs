module Assignments.Data where

--------------------------------------
-- This is how we define the datatype:

data MyDataName =
  MyDataConstructor1 | MyDataConstructor2 | MyDataConstructor3 -- it has three constructors

example1 :: MyDataName
example1 = MyDataConstructor1 -- this is an instance of MyDataName

example2 :: MyDataName
example2 = MyDataConstructor2 -- this too

-- we can use it as regular data, for example
-- here we put it into a list and reverse this list
example3 :: [MyDataName] 
example3 = reverse [MyDataConstructor1,MyDataConstructor2]

-- >>> example3
-- No instance for (Show MyDataName) arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_a17TU

{- ---------------------------

  Assignment 1

  Define your own datatype, that represents animals.
  It should cover at least dogs, cats, turtles, rats, mouses, fishes, canaries.

-}



{- ---------------------------

  Assignment 2

  Fill holes using your datatype and mind.

-}

mikkieTheCat :: _
mikkieTheCat = undefined

mikeTheDog :: _
mikeTheDog = undefined

jacklineTheFish :: _
jacklineTheFish = undefined

jackTheDog :: _
jackTheDog = undefined

-- Sandra has a dog Jack and a cat Mikkie
sandraPets :: [_]
sandraPets = undefined

{- ---------------------------

  Assignment 3

-}