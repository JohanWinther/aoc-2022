#!/usr/bin/env runhaskell

import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Function ((&))
import Data.List (foldl')
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> String
part1 =
  show
    . topCrates
    . uncurry moveStacks
    . bimap parseStacks (fmap parseMove . drop 1)
    . break null
    . lines

maxStacks :: Int
maxStacks = 9

parseStacks :: [String] -> [Stack]
parseStacks =
  foldr parse (replicate maxStacks Nothing)
    . init
  where
    parse :: String -> [Stack] -> [Stack]
    parse next stacks =
      ( \(stack, letter) ->
          if letter == ' '
            then stack
            else case stack of
              Nothing ->
                Just $ Crate letter Nothing
              Just bottom ->
                Just $ bottom `add` Crate letter Nothing
      )
        <$> zip stacks letters
      where
        letters = (next !!) <$> positions

    positions :: [Int]
    positions = cratePosition <$> take maxStacks (iterate succ 0)
    cratePosition n = 4 * n + 1

topCrates :: [Stack] -> [Maybe Crate]
topCrates stacks = fmap top <$> stacks

parseMove :: String -> (Int, (Int, Int))
parseMove =
  bimap
    read
    ( bimap read (read . drop 3)
        . splitOn ' '
        . drop 5
    )
    . splitOn ' '
    . drop 5

moveStacks :: [Stack] -> [(Int, (Int, Int))] -> [Stack]
moveStacks =
  foldl' applyMove
  where
    applyMove :: [Stack] -> (Int, (Int, Int)) -> [Stack]
    applyMove stacks (amount, (from, to)) =
      setAt to longerStack $
        setAt from shorterStack $
          stacks
      where
        shorterStack = remove amount $ stacks !! from

        remove n Nothing = Nothing
        remove n (Just (Crate mark above)) = iterate remove

    setAt :: Int -> a -> [a] -> [a]
    setAt i a ls
      | i < 0 = ls
      | otherwise = go i ls
      where
        go 0 (_ : xs) = a : xs
        go n (x : xs) = x : go (n -1) xs
        go _ [] = []

splitOn :: Char -> String -> (String, String)
splitOn c = second (drop 1) . span (c /=)

type Stack = Maybe Crate

top :: Crate -> Crate
top (Crate mark above) = case above of
  Nothing ->
    Crate mark above
  Just crateAbove ->
    top crateAbove

data Crate = Crate
  { mark :: Char,
    above :: Maybe Crate
  }
  deriving (Eq)

instance Show Crate where
  show (Crate mark above) = maybe "" show above <> "\n[" <> [mark] <> "]"

add :: Crate -> Crate -> Crate
add (Crate mark above) new =
  case above of
    Nothing ->
      Crate mark (Just new)
    Just crateAbove ->
      Crate mark (Just (crateAbove `add` new))

part2 :: String -> Int
part2 _ =
  0
