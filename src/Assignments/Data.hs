{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
module Assignments.Data where
import Data.Kind (Type)
import APrelude (if_then_else)

--------------------------------------
-- This is how we define the datatype:

data MyDataName =
  MyDataCons666tructor1 | MyDataConstructor2 | MyDataConstructor3
  -- it has three constructors

-- this brings into our scope all three data constructors. For example:
_ = MyDataCons666tructor1 :: MyDataName

example1 :: MyDataName
example1 = MyDataCons666tructor1 -- this is an instance of MyDataName

example2 :: MyDataName
example2 = MyDataConstructor2 -- this too

-- we can use it as regular data, for example
-- here we put it into a list and reverse this list
example3 :: [MyDataName] 
example3 = reverse [MyDataCons666tructor1,MyDataConstructor2]

-- >>> deriving instance Show MyDataName -- we need this to print out MyDataName terms
-- >>> example3
-- [MyDataConstructor2,MyDataConstructor1]

{- ---------------------------

  Assignment 1

  Define your own datatype, that represents animals.
  It should cover at least dogs, cats, turtles, rats, mouses, fishes, canaries.

-}

data Animal =
  Dog | Cat | Turtle | Rat | Mouse | Fish | Canary

{- ---------------------------

  Assignment 2

  Fill holes using your datatype and mind.

-}

mikkieTheCat :: Animal
mikkieTheCat = Cat

mikeTheDog :: Animal
mikeTheDog = Dog

jacklineTheFish :: Animal
jacklineTheFish = Fish

jackTheDog :: Animal
jackTheDog = Dog

-- Sandra has a dog Jack and a cat Mikkie
sandraPets :: [Animal]
sandraPets = [jackTheDog, mikkieTheCat]

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
eatsKgOfFoodPerDay :: Animal -> Float
eatsKgOfFoodPerDay Dog = 7
eatsKgOfFoodPerDay Cat = 1
eatsKgOfFoodPerDay Turtle = 3
eatsKgOfFoodPerDay Rat = 0.3
eatsKgOfFoodPerDay Mouse = 0.2
eatsKgOfFoodPerDay Fish = 0.003
eatsKgOfFoodPerDay Canary = 0.1

eatsKgOfFoodPerDay' :: Animal -> Float
eatsKgOfFoodPerDay' animal = case animal of
  Dog -> 7
  Cat -> 1
  Turtle -> 3
  Rat -> 0.3
  Mouse -> 0.2
  Fish -> 0.003
  Canary -> 0.1

{- ---------------------------

  Assignment 4

  Only cats and dogs do shed.

-}

-- | Whether certain kind of animals sheds or not

sheds :: Animal -> Bool
sheds Dog = True
sheds Cat = True
sheds _ = False

---------------------------
-- Data constructors may have arguments.

{- | The datatype represent an intruder that
 has been spotted (on camera or by staff)
  It's either was not identifed, or we know intruders identity number. 
-}
data SpottedIntruder
  = NotIdentified
  | IdentifiedAs CitizenIdentityNumber

-- | The description of a next step of catching the villain.
data NextStepToCatch
  = PutInWantedList Photo
    -- maybe someone will recognize him by photo and we get villain identity
  | AmbushAtRegistrationAddress CitizenIdentityNumber
    -- we know where he lives. Let's catch him up there
  | ApologizeAndWaitForNewCrimes
    -- because what can we do without having neither photo nor identity?

-- | Given maybe photo and spotted intruder data, calculates what to do to catch villain
nextStepToCatch :: Maybe Photo -> SpottedIntruder -> NextStepToCatch
nextStepToCatch (Just photo) _ = 
  PutInWantedList photo
nextStepToCatch _ (IdentifiedAs citizenIdentityNumber) = 
  AmbushAtRegistrationAddress citizenIdentityNumber
nextStepToCatch _ _ = 
  ApologizeAndWaitForNewCrimes


-- It's not important what's here, you don't need it.
data CitizenIdentityNumber = EuropeanFormat Integer | RussianFormat String 
data Photo = BitmapImage [[(Float,Float,Float)]]