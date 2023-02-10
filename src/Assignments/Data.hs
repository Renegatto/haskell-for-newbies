{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# LANGUAGE DerivingStrategies #-}
module Assignments.Data where
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.List (groupBy, sortBy, sortOn)

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

{- ---------------------------

  Assignment 4

  Only cats and dogs do shed.

-}

-- | Whether certain kind of animals sheds or not
sheds :: _ -> Bool
sheds = undefined

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
nextStepToCatch = undefined

nextStepToCatchIsReady :: Bool
nextStepToCatchIsReady = False
 
-- It's not important what's here, you don't need it.
data CitizenIdentityNumber = EuropeanFormat Integer | RussianFormat String 
data Photo = BitmapImage [[(Float,Float,Float)]]



--------------------------
-- RECORD SYNTAX



data LearningStage
  = IntroToVisualisation
  | MarketingLearning
  | EarningMarathon
  deriving stock Show

{-
 By use of "record syntax" we may ask Haskell
to generate field accessors for our datatype

-}

data Visualisators = VisualisatorsGroup
  { curator :: Teacher, captain :: Student,
    students :: [Student],
    stage :: LearningStage
  }
  deriving stock Show

-- | Now we have in scope a bunch of functions
-- that ease the data access of this record type:
_ = curator :: Visualisators -> Teacher
_ = stage :: Visualisators -> LearningStage


data Teacher = SomeTeacher
  { teacherName :: String
  , teacherWorkingExperience :: Int 
  , teacherAchievements :: [String]
  , teacherAge :: Int
  }
  deriving stock Show

data Student = MkStudent
  { studentName :: String
  , studentAge :: Int
  , studentPerformance :: Float
  }
  deriving stock Show

age :: Either Teacher Student -> Int
-- We may use `teacherAge` field accessor to extract age field
age (Left teacher) = teacherAge teacher
-- Or we may use the field name in pattern-matching to extract the field
age (Right MkStudent { studentAge = theAge }) = theAge

name :: Either Teacher Student -> String
name = undefined

captainAge :: Visualisators -> Int
-- field accessors are function and we can compose them
captainAge = studentAge . captain

curatorName :: Visualisators -> String
curatorName = undefined

polina :: Student
{- We can construct records completely ignoring record syntax
 But this way you should keep in mind the fields order and meaning,
 which is complicated.
-}
polina = MkStudent "Polina" 18 0

maxim :: Student
{- Instead, we can use record syntax.
 It's more easy to understand what those values means, isn't it?
 Note also, that since fields are named, when we create the term
 using record syntax we don't care about fields order.
-}
maxim = MkStudent
  { studentName = "Maksim Skvorcov",
    studentPerformance = 0.56,
    studentAge = 28
  }

{-

Strelkov Gennadiy Petrovich,
born in 1987,
working as 3D visualisation teacher since 2018,
3 of his students opened their businesses after he taught them.
Has 8 awards from world-famous 3D-visualisation competitions.

-}
gennadiy :: Teacher
gennadiy = undefined

{- We define teacher performance as amount of learning stages multiplied by
 the average student performance divided by number of current learning stage.
-}
teacherPerformance :: Visualisators -> Float
teacherPerformance = undefined

{- | Given the learning group and the student name,
returns the student learning stage, if student is in given learning group.
-}
studentStage :: Visualisators -> String -> Maybe LearningStage
studentStage = undefined 

---- More on pattern-matching!

-- Note that we can also pattern-match against constructors that are operators.
groupVisualisators :: LearningStage -> Teacher -> [Student] -> Maybe Visualisators
-- Here we pattern-match empty list
groupVisualisators _ _ [] = Nothing
-- And here we pattern-match list that has the first element (firstStudent).
groupVisualisators currentStage teacher (firstStudent : otherStudents) =
  Just
    (VisualisatorsGroup
      { curator = teacher
      , captain = firstStudent
      , students = otherStudents
      , stage = currentStage
      })

-- | The function that extracts first student from list of students, if any.
-- CONSTRAINTS: do not use any functions in the definition, except of constructors. 
veryFirstStudent :: [Student] -> Maybe Student
veryFirstStudent = undefined

-- | The function that returns three first students, if there is any
firstThreeStudents :: [Student] -> Maybe (Student,Student,Student)
firstThreeStudents = undefined

{-
  If list contains exactly one element - return it.
  If it contains more than two - return all of them.
  If it contains exactly two - throw them away!

  CONSTRAINT - do not use any functions except constructors, do not use list-syntax patterns
-}
eitherOneOrMoreThanTwo :: [a] -> Maybe (Either a [a])
eitherOneOrMoreThanTwo = undefined

{-

FYI, for lists haskell allows to pattern-match on list-syntax:

-}

-- | Checks whether list contains exactly four elements or not
listOfFourElements :: [a] -> Bool
listOfFourElements [_,_,_,_] = True
listOfFourElements _ = False

{- ---------------------------

  Assignment 6

  Group new students.

  There are several criterias we guided by when grouping students:
  - The higher student age - the higher teacher age must be
  - Difference in student's age inside the group must be minimized
  - Each teacher must have a students if it's possible
  - All groups must have equal size or close to it

  Hints: use hoogle, especially module `Data.List`, `Prelude`, `Data.Maybe` and friends.

-}

groupNewbies :: [Teacher] -> [Student] -> [Visualisators]
groupNewbies = undefined

chunksOf :: Int -> [a] -> [[a]]
chunksOf chunkSize =
  fmap (fmap snd)
  . groupBy ((==) `on` fst)
  . zip (replicate chunkSize =<< [0 :: Int ..])

