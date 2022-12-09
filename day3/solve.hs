#!/usr/bin/env runhaskell

import Data.List (foldl', intersect, zip)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 =
  foldl' parse 0
    . lines
  where
    parse :: Int -> String -> Int
    parse current =
      (current +)
        . priority
        . head
        . uncurry intersect
        . splitHalf

    splitHalf s = splitAt (length s `div` 2) s

part2 :: String -> Int
part2 =
  fst
    . foldl' parse (0, "")
    . zip (iterate succ 0)
    . (\xs -> xs ++ [""])
    . lines
  where
    parse :: (Int, String) -> (Int, String) -> (Int, String)
    parse (prioritySum, commonItems) (lineNumber, rucksack)
      | lineNumber `mod` 3 == 0 = (prioritySum + if null commonItems then 0 else priority (head commonItems), rucksack)
      | otherwise = (prioritySum, commonItems `intersect` rucksack)

priority :: Char -> Int
priority c
  | 'A' <= c && c <= 'Z' = fromEnum c - 64 + 26
  | 'a' <= c && c <= 'z' = fromEnum c - 96
  | otherwise = error "no priority for character"
