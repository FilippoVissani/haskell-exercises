module Homework1 where

-- Exercise 1
-- Convert positive Integers to a list of digits
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (div n 10) ++ [mod n 10]

-- Convert positive Integers to a reversed list of digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = mod n 10 : (toDigitsRev (div n 10))

-- Exercise 2
-- Double every other number
doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft []        = []
doubleEveryOtherLeft [h]       = [h]
doubleEveryOtherLeft [h1, h2]  = [h1, 2 * h2]
doubleEveryOtherLeft (h1:h2:t) = h1:2 * h2:doubleEveryOtherLeft(t)

-- Double every other number beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleEveryOtherLeft (reverse (l)))

-- Exercise 3
-- Calculate the sum of all digits of a list
sumDigits :: [Integer] -> Integer
sumDigits []    = 0
sumDigits (h:t) = (sum (toDigits (h))) + sumDigits t

-- Exercise 4
-- Indicates whether an Integer could be a valid credit card number
validate :: Integer -> Bool
validate n = ((mod (sumDigits (doubleEveryOther (toDigits n))) 10)) == 0
