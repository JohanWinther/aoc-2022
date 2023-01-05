#!/usr/bin/env runhaskell

import Data.Bifunctor (Bifunctor (bimap))
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

data Position = Position Int Int
  deriving (Eq)

instance Show Position where
  show (Position x y) = "(" <> show x <> ", " <> show y <> ")"

instance Semigroup Position where
  (Position x1 y1) <> (Position x2 y2) = Position (x1 + x2) (y1 + y2)

instance Monoid Position where
  mempty = Position 0 0

instance Ord Position where
  compare :: Position -> Position -> Ordering
  compare (Position x1 y1) (Position x2 y2)
    | y1 < y2 = LT
    | y1 == y2 && x1 < x2 = LT
    | y1 == y2 && x1 == x2 = EQ
    | y1 == y2 && x1 > x2 = GT
    | y1 > y2 = GT

negative :: Position -> Position
negative (Position x y) = Position (- x) (- y)

(.-) :: Position -> Position -> Position
(.-) p1 p2 = p1 <> negative p2

data Direction
  = U
  | D
  | R
  | L
  deriving (Read, Show)

type Rope = (Position, Position)

type TailVisits = Set Position

type State = (Rope, TailVisits)

start :: State
start =
  ( mempty,
    Set.fromList [mempty]
  )

part1 :: String -> Int
part1 =
  Set.size
    . snd
    . foldl (flip addMove) start
    . concatMap readMoves
    . lines

readMoves :: String -> [Direction]
readMoves =
  (\(d, n) -> replicate n d)
    . bimap read read
    . splitAt 1

addMove :: Direction -> State -> State
addMove d (current@(currentTail, currentHead), visited) =
  let newHead = moveHead d currentHead
      newTail = moveTail currentTail newHead currentHead
   in ( (newTail, newHead),
        Set.insert newTail visited
      )

moveHead :: Direction -> Position -> Position
moveHead U (Position x y) = Position x (y + 1)
moveHead D (Position x y) = Position x (y - 1)
moveHead R (Position x y) = Position (x + 1) y
moveHead L (Position x y) = Position (x - 1) y

moveTail :: Position -> Position -> Position -> Position
moveTail currentTail newHead currentHead =
  let Position x y = newHead .- currentTail
   in if abs x <= 1 && abs y <= 1
        then currentTail
        else currentHead

part2 :: String -> Int
part2 _ =
  0
