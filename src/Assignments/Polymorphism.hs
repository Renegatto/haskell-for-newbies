{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
module Assignments.Polymorphism where

import Prelude

-- | Find the most general type for this function.
swap :: (_,_) -> (_,_)
swap (a,b) = (b,a)

-- | Find the most general type for this function.
headToLast :: [Int] -> [Int]
headToLast (x:xs) = xs ++ [x]
headToLast [] = []

data SomeType
  = IsInt Int
  | IsBool Bool
  | IsString String

-- | Fill the gaps in function types
manyShows :: [SomeType] -> [_ -> String] -> [String]
manyShows xs shows' =
  zipWith f shows' xs
  where
    f :: (_ -> String) -> SomeType -> String 
    f show' x = case x of
      IsInt n -> show' n
      IsBool b -> show' b
      IsString s -> show' s

maybeNatTrans :: forall a. Maybe a -> [a]
maybeNatTrans (Just x) = [x]
maybeNatTrans Nothing = []

eitherNatTrans :: forall a. Either () a -> Maybe a
eitherNatTrans (Left _) = Nothing
eitherNatTrans (Right x) = Just x

natTransMaybeEither :: (Maybe _ -> Either () _) -> (Either () Int, Either () String)
natTransMaybeEither natTrans = (natTrans Nothing, natTrans $ Just "Hey")

withNatTrans :: forall f g. (f _ -> g _) -> (forall a. [f a] -> [g a])
withNatTrans = fmap
