#!/usr/bin/env runhaskell

import Data.List (foldl', nub)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 =
  general 4

part2 :: String -> Int
part2 =
  general 14

general :: Int -> String -> Int
general markerLength signal =
  succ
    . snd
    . last
    . takeWhile (\(marker, _) -> length marker /= length (nub marker))
    . scanl
      (\(potentialMarker, latestCharacterNum) next -> (next : take (markerLength - 1) potentialMarker, succ latestCharacterNum))
      (reverse $ take markerLength signal, markerLength - 1)
    . drop (markerLength - 1)
    $ signal
