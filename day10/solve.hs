#!/usr/bin/env runhaskell

import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Num (Natural)

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

parse :: String -> [(Int, Int)]
parse =
  zip [1 ..]
    . reverse
    . foldl
      ( \(x : acc) i ->
          case i of
            Noop ->
              x : x : acc
            Add n ->
              (x + n) : x : x : acc
      )
      [initialValue]
    . fmap fromString
    . lines
  where
    initialValue = 1

part1 :: String -> Int
part1 =
  sum
    . fmap (uncurry (*))
    . filter (\(cycle, _) -> cycle `elem` lookupCycles)
    . parse
  where
    lookupCycles = take 6 $ iterate (+ 40) 20

data Instruction
  = Noop
  | Add Int
  deriving (Show)

fromString :: String -> Instruction
fromString "noop" = Noop
fromString ('a' : 'd' : 'd' : 'x' : ' ' : n) = Add (read n)

part2 :: String -> CRT
part2 = draw . parse

newtype CRT = CRT (Set Int)

width :: Int
width = 40

height :: Int
height = 6

instance Show CRT where
  show (CRT lit) =
    unlines $
      fmap (\n -> if n `Set.member` lit then '#' else '.')
        <$> grid
    where
      row n = [n * width .. (n + 1) * width - 1]
      grid :: [[Int]]
      grid = row <$> [0 .. height - 1]

draw :: [(Int, Int)] -> CRT
draw =
  CRT
    . foldl
      ( \lit (cycle, x) ->
          let drawX = (cycle - 1) `mod` width
           in if drawX == x -1
                || drawX == x
                || drawX == x + 1
                then (cycle - 1) `Set.insert` lit
                else lit
      )
      mempty
