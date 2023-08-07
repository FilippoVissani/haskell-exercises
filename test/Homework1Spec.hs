module Homework1Spec where

import Test.HUnit
import Homework1

testToDigits1 :: Test
testToDigits1 = TestCase (assertEqual "should be [1,2,3,4]" [1,2,3,4] (toDigits 1234))

testToDigits2 :: Test
testToDigits2 = TestCase (assertEqual "should be []" [] (toDigits 0))

testToDigits3 :: Test
testToDigits3 = TestCase (assertEqual "should be []" [] (toDigits (-17)))

testToDigitsRev :: Test
testToDigitsRev = TestCase (assertEqual "should be [4,3,2,1]" [4,3,2,1] (toDigitsRev 1234))

testDoubleEveryOther1 :: Test
testDoubleEveryOther1 = TestCase (assertEqual "should be [16,7,12,5]" [16,7,12,5] (doubleEveryOther [8,7,6,5]))

testDoubleEveryOther2 :: Test
testDoubleEveryOther2 = TestCase (assertEqual "should be [1,4,3]" [1,4,3] (doubleEveryOther  [1,2,3]))

testSumDigits :: Test
testSumDigits = TestCase (assertEqual "should be 1 + 6 + 7 + 1 + 2 + 5" (1 + 6 + 7 + 1 + 2 + 5) (sumDigits [16,7,12,5]))

testValidate1 :: Test
testValidate1 = TestCase (assertEqual "should be True" True (validate 4012888888881881))

testValidate2 :: Test
testValidate2 = TestCase (assertEqual "should be False" False (validate 4012888888881882))

testHanoi1 :: Test
testHanoi1 = TestCase (assertEqual "should be [(a,c), (a,b), (c,b)]" [("a","c"), ("a","b"), ("c","b")] (hanoi 2 "a" "b" "c"))

testHanoi2 :: Test
testHanoi2 = TestCase (assertEqual "should be [(a,b), (a,c), (b,c), (a,b), (c,a), (c,b), (a,b)]" [("a","b"), ("a","c"), ("b","c"), ("a","b"), ("c","a"), ("c","b"), ("a","b")] (hanoi 3 "a" "b" "c"))

homework1Spec :: Test
homework1Spec = TestList [
    TestLabel "testToDigits1" testToDigits1,
    TestLabel "testToDigits2" testToDigits2,
    TestLabel "testToDigits3" testToDigits3,
    TestLabel "testToDigitsRev" testToDigitsRev,
    TestLabel "testDoubleEveryOther1" testDoubleEveryOther1,
    TestLabel "testDoubleEveryOther2" testDoubleEveryOther2,
    TestLabel "testSumDigits" testSumDigits,
    TestLabel "testValidate1" testValidate1,
    TestLabel "testValidate2" testValidate2,
    TestLabel "testHanoi" testHanoi1
    ]
