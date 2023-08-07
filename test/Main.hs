module Main where

import Test.HUnit
import qualified System.Exit as Exit
import Homework1Spec

tests :: Test
tests = homework1Spec

main :: IO()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
