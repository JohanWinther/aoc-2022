#!/usr/bin/env runhaskell

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 =
  general (0, 0) max id

part2 :: String -> Int
part2 =
  general ((0, 0, 0), 0) replace sumTriple
  where
    replace :: (Int, Int, Int) -> Int -> (Int, Int, Int)
    replace (first, second, third) new
      | new >= first = (new, first, second)
      | new >= second = (first, new, second)
      | new > third = (first, second, new)
      | otherwise = (first, second, third)

    sumTriple (a, b, c) = a + b + c

general :: (a, Int) -> (a -> Int -> a) -> (a -> Int) -> String -> Int
general start replace extract =
  extract
    . fst
    . foldr parse start
    . lines
  where
    parse next (largest, current) =
      case reads next of
        [(new, "")] ->
          (largest, current + new)
        _ ->
          (replace largest current, 0)
