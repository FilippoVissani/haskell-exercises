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

main = do
    putStrLn ("toDigits 1234 should be [1,2,3,4]")
    print (toDigits 1234 == [1,2,3,4])
    putStrLn ("toDigitsRev 1234 should be [4,3,2,1]")
    print (toDigitsRev 1234 == [4,3,2,1])
    putStrLn ("toDigits 0 should be []")
    print (toDigits 0 == [])
    putStrLn ("toDigits (-17) should be []")
    print (toDigits (-17) == [])
    putStrLn ("doubleEveryOther [8,7,6,5] should be [16,7,12,5]")
    print (doubleEveryOther [8,7,6,5] == [16,7,12,5])
    putStrLn ("doubleEveryOther  [1,2,3] should be [1,4,3]")
    print (doubleEveryOther  [1,2,3] == [1,4,3])
    putStrLn ("sumDigits [16,7,12,5] should be 1 + 6 + 7 + 1 + 2 + 5")
    print (sumDigits [16,7,12,5] == 1 + 6 + 7 + 1 + 2 + 5)
    putStrLn ("validate 4012888888881881 should be True")
    print (validate 4012888888881881 == True)
    putStrLn ("validate 4012888888881882 should be False")
    print (validate 4012888888881882 == False)
