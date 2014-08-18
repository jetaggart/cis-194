module Homework where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits i
    | i <= 0 = []
    | otherwise = map (toInteger . digitToInt) $ show i


toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith double [0..] . reverse
    where
        double :: Integer -> Integer -> Integer
        double index number
            | index `mod` 2 == 0 = number
            | otherwise = number * 2

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate number = calculateSum number `mod` 10 == 0
    where calculateSum = sumDigits . doubleEveryOther . toDigits
