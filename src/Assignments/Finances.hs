{-# LANGUAGE DataKinds #-}
module Assignments.Finances where
import Finances (Staff,  Post(Directors))
import Finances.Types (USD, Worker, Performance,)

averageSalary :: [Worker] -> USD
averageSalary = undefined

performanceOf :: Worker -> Performance
performanceOf = undefined

workersNames :: [Worker] -> [String]
workersNames = undefined

{- |
Average name is defined as the most frequent name (if any)
For the rest of properties average is obvious
-}
averageWorker :: [Worker] -> Worker
averageWorker = undefined

-- final test

workersAmount :: Staff post -> USD
workersAmount = undefined

totalCompanySalary :: Staff 'Directors -> USD
totalCompanySalary = undefined