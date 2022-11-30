{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Finances.Types (USD (..), Worker(..), Performance (..)) where
import Data.Kind (Type)

type USD :: Type
newtype USD = MkUSD { getUSD :: Double }
  deriving stock (Eq, Show)
  deriving newtype (Num,Enum,Fractional)

type Performance :: Type
data Performance =
  MkPerformance
  { outcome :: USD
  , salary :: USD
  }
  deriving stock (Eq,Show)

type Worker :: Type
data Worker = MkWorker
  { name :: String
  , outcome :: USD
  , salary :: USD
  }
