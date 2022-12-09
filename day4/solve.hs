#!/usr/bin/env runhaskell

import Data.Bifunctor (Bifunctor (bimap, second))
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 =
  general fullyContained
  where
    fullyContained (fl, fh) (sl, sh) = fl <= sl && sl <= fh && sh <= fh

part2 :: String -> Int
part2 =
  general partiallyContained
  where
    partiallyContained (fl, fh) (sl, sh) = fl <= sl && sl <= fh

general :: ((Int, Int) -> (Int, Int) -> Bool) -> String -> Int
general criteria =
  length
    . filter (\r -> uncurry (flip criteria) r || uncurry criteria r)
    . fmap parseRanges
    . lines

parseRanges :: String -> ((Int, Int), (Int, Int))
parseRanges =
  bimap readPair readPair
    . splitOn ','
  where
    readPair =
      bimap read read
        . splitOn '-'

    splitOn :: Char -> String -> (String, String)
    splitOn c = second (drop 1) . span (c /=)
