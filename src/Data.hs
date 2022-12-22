{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-missing-kind-signatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
module Data where
import Data.Kind (Type)
import Assignments.Data
import Data.Functor.Contravariant (Predicate)
import Data.Monoid (Any(Any), All(All))

nextStepToCatch' :: Maybe Photo -> SpottedIntruder -> NextStepToCatch
_ = nextStepToCatch :: Maybe Photo -> SpottedIntruder -> NextStepToCatch

prop_nextStepToCatch_is_valid :: Maybe Photo -> SpottedIntruder -> Result All
prop_nextStepToCatch_is_valid pic spot =
  MkResult $ All $ nextStepToCatch' pic spot == nextStepToCatch pic spot

test_nextStepToCatch_is_valid :: [Maybe Photo] -> [SpottedIntruder] -> All
test_nextStepToCatch_is_valid = onMultiple @All prop_nextStepToCatch_is_valid


-- >>> test
-- All {getAll = False}

test :: All
test = test_nextStepToCatch_is_valid
  [ bmp []
  , Nothing
  , bmp [[(3,220,3)],[(30,255,0)]]
  ]
  [ NotIdentified
  , IdentifiedAs (EuropeanFormat 234234252)
  , IdentifiedAs (EuropeanFormat 123)
  , IdentifiedAs (RussianFormat "KB124214C")
  ]
  where
    bmp = Just . BitmapImage

onMultiple :: forall m f. OnMultiple f m => f -> Rest f m
onMultiple f = hoo @f @m [f]

newtype Result m = MkResult { getResult :: m }
  deriving newtype (Semigroup,Monoid)

class Monoid m => OnMultiple f m where
  type Rest f m
  hoo :: [f] -> Rest f m

instance Monoid m => OnMultiple (Result m) m where
   type Rest (Result m) m = m
   hoo :: [Result m] -> m
   hoo = foldMap getResult

instance (Monoid m, OnMultiple f m) => OnMultiple (a -> f) m where
  type Rest (a -> f) m = [a] -> Rest f m
  hoo :: [a -> f] -> [a] -> Rest f m
  hoo af as = hoo @f @m $ af <*> as

keks :: Int -> String -> Bool -> Result Any
keks n s b = MkResult $ Any $ b || n > 0 || length s == 3

h :: [Int] -> [String] -> [Bool] -> Any
h = onMultiple @Any keks

deriving stock instance Eq Photo
deriving stock instance Eq CitizenIdentityNumber
deriving stock instance Eq SpottedIntruder
deriving stock instance Eq NextStepToCatch

nextStepToCatch' _ (IdentifiedAs uid) = AmbushAtRegistrationAddress uid
nextStepToCatch' (Just photo) _ = PutInWantedList photo
nextStepToCatch' Nothing _ = ApologizeAndWaitForNewCrimes
