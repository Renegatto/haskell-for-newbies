{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module APrelude (if_then_else,List) where
import GHC.TypeLits (TypeError(..), ErrorMessage (Text, (:<>:), ShowType))
if_then_else :: Bool -> a -> a -> a
if_then_else True ifTrue _ = ifTrue
if_then_else _ _ ifFalse = ifFalse
type List = []
instance 
  TypeError (
    'Text "Expected number, got function '"
    :<>: 'ShowType (a -> b)
    :<>: 'Text "'")
  => Num (a -> b)
instance 
  TypeError (
    'Text "Cannot compare functions of type '"
    :<>: 'ShowType (a -> b)
    :<>: 'Text "'")
  => Eq (a -> b)
instance 
  TypeError (
    'Text "Cannot compare functions of type '"
    :<>: 'ShowType (a -> b)
    :<>: 'Text "'")
  => Ord (a -> b)
instance 
  TypeError (
    'Text "Cannot show functions of type '"
    :<>: 'ShowType (a -> b)
    :<>: 'Text "'")
  => Show (a -> b)