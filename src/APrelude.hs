module APrelude (if_then_else) where

if_then_else :: Bool -> a -> a -> a
if_then_else True ifTrue _ = ifTrue
if_then_else _ _ ifFalse = ifFalse