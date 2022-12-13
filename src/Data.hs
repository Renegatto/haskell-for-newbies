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
module Data where
import Data.Kind (Type)
import Assignments.Data
import Data.Functor.Contravariant (Predicate)
import Data.Monoid (Any(Any))
nextStepToCatch' :: Maybe Photo -> SpottedIntruder -> NextStepToCatch
nextStepToCatch' _ (IdentifiedAs uid) = AmbushAtRegistrationAddress uid
nextStepToCatch' (Just photo) _ = PutInWantedList photo
nextStepToCatch' Nothing _ = ApologizeAndWaitForNewCrimes
