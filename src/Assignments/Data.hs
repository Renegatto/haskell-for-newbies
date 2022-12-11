{-# LANGUAGE StandaloneDeriving #-}
module Assignments.Data where

--------------------------------------
-- This is how we define the datatype:

data MyDataName =
  MyDataConstructor1 | MyDataConstructor2 | MyDataConstructor3 -- it has three constructors

-- this brings into our scope all three data constructors. For example:
_ = MyDataConstructor1 :: MyDataName

example1 :: MyDataName
example1 = MyDataConstructor1 -- this is an instance of MyDataName

example2 :: MyDataName
example2 = MyDataConstructor2 -- this too

-- we can use it as regular data, for example
-- here we put it into a list and reverse this list
example3 :: [MyDataName] 
example3 = reverse [MyDataConstructor1,MyDataConstructor2]

-- >>> deriving instance Show MyDataName -- we need this to print out MyDataName terms
-- >>> example3
-- [MyDataConstructor2,MyDataConstructor1]

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






---------------------------
-- You also can pattern-match your datatypes in functions.


data DirectionToGo = GoLeft | GoRight

-- Say, we expect function "whereShouldIGo" to behave this way:

-- >>> deriving instance Show DirectionToGo
-- >>> whereShouldIGo GoRight
-- "You go right."
-- >>> whereShouldIGo GoLeft
-- "You go left."

-- Gladly, we can specify this directly:

whereShouldIGo :: DirectionToGo -> String
whereShouldIGo GoLeft = "You go left."
whereShouldIGo GoRight = "You go right."

{- Patterns matches from top to down, this is important.
  if pattern doesn't match term, next pattern will compared against it, and so on.
-}

data Number = One | Two | Three | Four

isItOneOrThree :: Number -> String
isItOneOrThree Three = "Yes, it is three"
isItOneOrThree One = "Yes, it is one"
-- this is the "wildcard pattern", it matches everything. Here, it matches Two and Four.
isItOneOrThree _ = "Nope, it's something else"

-- >>> deriving instance Show DirectionToGo
-- >>> isItOneOrThree Four
-- "Nope, it's something else"
-- >>> isItOneOrThree Three
-- "Yes, it is three"

{- ---------------------------

  Assignment 3

  Given this table, define function "eatsKgOfFoodPerDay"
 
  animal   | eats food per day, Kg
  ------------------------
  dogs     | 7
  cats     | 1
  turtles  | 3
  rats     | 0.3
  mouses   | 0.2
  fishes   | 0.003
  canaries | 0.1

-}

-- | How much given animal eats Kg of food per day
eatsKgOfFoodPerDay :: _ -> Float
eatsKgOfFoodPerDay = undefined
