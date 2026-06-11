{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
{-# OPTIONS_GHC -Werror=partial-type-signatures #-}
module Assignments.Polymorphism where

import Prelude

-- | Find the most general type for this function.
swap :: forall a b . (a,b) -> (b,a)
swap (a,b) = (b,a)

-- | Find the most general type for this function.
headToLast :: forall a. [a] -> [a]
headToLast (x:xs) = xs ++ [x]
headToLast [] = []

data SomeType
  = IsInt Int
  | IsBool Bool
  | IsString String

-- | Fill the gaps in function types
manyShows :: [SomeType] -> [forall a . Show a => a -> String ] -> [String]
manyShows xs shows' =
  zipWith f shows' xs
  where
    f :: (forall a . Show a => a -> String) -> SomeType -> String 
    f show' x = case x of
      IsInt n -> show' n
      IsBool b -> show' b
      IsString s -> show' s

impl = manyShows [IsInt 55, IsBool True] [ show , show ]

maybeNatTrans :: forall a. Maybe a -> [a]
maybeNatTrans (Just x) = [x]
maybeNatTrans Nothing = []

eitherNatTrans :: forall a. Either () a -> Maybe a
eitherNatTrans (Left _) = Nothing
eitherNatTrans (Right x) = Just x

natTransMaybeEither :: (forall a . Maybe a -> Either () a) -> (Either () Int, Either () String)
natTransMaybeEither natTrans = (natTrans Nothing, natTrans $ Just "Hey")

withNatTrans :: forall f g. (forall a'. f a' -> g a' ) -> (forall a. [f a] -> [g a])
withNatTrans = fmap
