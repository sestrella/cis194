module CIS194.Homework01
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  ) where

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0     = toDigits' n []
  | otherwise = []

toDigits' :: Integer -> [Integer] -> [Integer]
toDigits' 0 xs = xs
toDigits' n xs = toDigits' (n `div` 10) (n `mod` 10 : xs)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = undefined
