module Main where

import Test.HUnit
import qualified System.Exit as Exit
import Homework1

--main = do
--    putStrLn ("toDigits 1234 should be [1,2,3,4]")
--    print (toDigits 1234 == [1,2,3,4])
--    putStrLn ("toDigitsRev 1234 should be [4,3,2,1]")
--    print (toDigitsRev 1234 == [4,3,2,1])
--    putStrLn ("toDigits 0 should be []")
--    print (toDigits 0 == [])
--    putStrLn ("toDigits (-17) should be []")
--    print (toDigits (-17) == [])
--    putStrLn ("doubleEveryOther [8,7,6,5] should be [16,7,12,5]")
--    print (doubleEveryOther [8,7,6,5] == [16,7,12,5])
--    putStrLn ("doubleEveryOther  [1,2,3] should be [1,4,3]")
--    print (doubleEveryOther  [1,2,3] == [1,4,3])
--    putStrLn ("sumDigits [16,7,12,5] should be 1 + 6 + 7 + 1 + 2 + 5")
--    print (sumDigits [16,7,12,5] == 1 + 6 + 7 + 1 + 2 + 5)
--    putStrLn ("validate 4012888888881881 should be True")
--    print (validate 4012888888881881 == True)
--    putStrLn ("validate 4012888888881882 should be False")
--    print (validate 4012888888881882 == False)

test1 :: Test
test1 = TestCase (assertEqual "should be [1,2,3,4]" [1,2,3,4] (toDigits 1234))

test2 :: Test
test2 = TestCase (assertEqual "should be [4,3,2,1]" [4,3,2,1] (toDigitsRev 1234))

test3 :: Test
test3 = TestCase (assertEqual "should be []" [] (toDigits 0))

test4 :: Test
test4 = TestCase (assertEqual "should be []" [] (toDigits (-17)))

test5 :: Test
test5 = TestCase (assertEqual "should be [16,7,12,5]" [16,7,12,5] (doubleEveryOther [8,7,6,5]))

test6 :: Test
test6 = TestCase (assertEqual "should be [1,4,3]" [1,4,3] (doubleEveryOther  [1,2,3]))

test7 :: Test
test7 = TestCase (assertEqual "should be 1 + 6 + 7 + 1 + 2 + 5" (1 + 6 + 7 + 1 + 2 + 5) (sumDigits [16,7,12,5]))

test8 :: Test
test8 = TestCase (assertEqual "should be True" True (validate 4012888888881881))

test9 :: Test
test9 = TestCase (assertEqual "should be False" False (validate 4012888888881882))

tests :: Test
tests = TestList [
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test5" test5,
    TestLabel "test6" test6,
    TestLabel "test7" test7,
    TestLabel "test8" test8,
    TestLabel "test9" test9
    ]

main :: IO()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
