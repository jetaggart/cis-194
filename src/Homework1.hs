module Homework1 where

import           Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits i
    | i <= 0 = []
    | otherwise = map (toInteger . digitToInt) $ show i


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


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = step1 ++ step2 ++ step3
    where
        step1 = hanoi (n-1) a c b
        step2 = [(a, b)]
        step3 = hanoi (n-1) c b a

