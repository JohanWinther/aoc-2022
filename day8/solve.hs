#!/usr/bin/env runhaskell

import Data.Bifunctor (Bifunctor (bimap, second))
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

-- part1 :: String -> Int
part1 =
  calculateVisibility
    . Map.fromList
    . readGrid

readGrid :: String -> [(Position, Tree)]
readGrid =
  concatMap (\(y, line) -> (\(x, h) -> (Position x y, tree $ read [h])) <$> indexed line)
    . indexed
    . lines

part2 :: String -> Int
part2 _ =
  0

type Forest = Map Position Tree

calculateVisibility :: Forest -> Forest
calculateVisibility =
  Map.adjust visible (Position 1 0)
    . Map.adjust (blocked (Position 0 1)) (Position 0 0)

data Position = Position Int Int
  deriving (Eq)

instance Show Position where
  show :: Position -> String
  show (Position x y) = "(" <> show x <> ", " <> show y <> ")"

instance Ord Position where
  compare :: Position -> Position -> Ordering
  compare (Position x1 y1) (Position x2 y2)
    | y1 < y2 = LT
    | y1 == y2 && x1 < x2 = LT
    | y1 == y2 && x1 == x2 = EQ
    | y1 == y2 && x1 > x2 = GT
    | y1 > y2 = GT

data Tree = Tree Height Visibility

instance Show Tree where
  show (Tree h Visible) = "^T" <> show h <> "^"
  show (Tree h Unknown) = "?T" <> show h <> "?"
  show (Tree h (BlockedBy _)) = "|T" <> show h <> "|"

type Height = Int

data Visibility = Visibility
  { up :: Int,
    left :: Int
  }

data Direction
  = Up
  | Down
  | Left
  | Right

tree :: Height -> Tree
tree h = Tree h Unknown

visible :: Tree -> Tree
visible (Tree h state) = Tree h Visible

block :: Direction -> Int -> Tree -> Tree
block d steps (Tree h Visible) = error "Cannot block, already set to visible"
block d steps (Tree h Unknown) = Tree h BlockedBy $ Map.fromList [()]
block d steps (Tree h (BlockedBy ps)) = Tree h (BlockedBy (p : ps))

-- Utilities
indexed :: [a] -> [(Int, a)]
indexed =
  zip (iterate succ 0)
