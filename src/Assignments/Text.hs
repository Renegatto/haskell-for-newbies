module Assignments.Text where

import Prelude
  ( Int
  , Num((+))
  , ($)
  , (.)
  , print
  , error
  , Bool (True,False)
  , Ord((>),(<),(>=),(<=))
  , undefined
  , Integer
  , (*)
  , (==)
  , (++)
  , (^)
  , String, flip
  )

-- :seti -interactive-print=Text.Show.Unicode.uprint

-- a -> b -> c
-- a -> (b -> c)

-- a b c
-- ((a b) c)

concat :: String -> String -> String
concat = (++)

-- | Восклицание
exclaim :: String -> String
exclaim = \s -> concat s "!"

-- | Поприветствовать
greet :: String -> String
greet = \name -> concat "Привет, мой друг " name


поля = "Полина"

_ = "Привет, мой друг Полина"

_ = "Привет, мой друг Полина. Игнат женился на Сергее?"

-- | Поженились
marry :: String -> (String -> String)
marry = \one second -> concat one (concat " женился на " second)

-- | Спросить
ask :: String -> String
ask = \question -> concat question "?"

закончить :: String -> String
закончить = flip concat "." 

черезПробел :: String -> String -> String
черезПробел = \a -> concat (concat a " ")

странныйВопрос = "А ты знала, что Евгений занимается евгеникой?"

-- * Assignment 2
{-
поШаблону "Полина" = "Привет, мой друг Полина. Игнат женился на Сергее?"
поШаблону "Витя" = "Привет, мой друг Витя. Игнат женился на Сергее?"
-}

задание2готово :: Bool
задание2готово = True

поШаблону :: String -> String
поШаблону = \name ->
  черезПробел (закончить (greet name)) (ask (marry "Игнат" "Сергее")) 
