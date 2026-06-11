module Utils (Id, identities) where

data Id = MkId Int
  deriving (Show, Eq, Ord)

