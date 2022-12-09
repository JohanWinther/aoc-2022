#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}

import Data.Bifunctor (Bifunctor (bimap))
import GHC.List (foldl')

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 contents =
  foldl' parse 0 $
    parseRound parseOpponent parsePlayer
      <$> lines contents
  where
    parse :: Int -> (Action, Action) -> Int
    parse points actions@(opponent, player) =
      points
        + scoreForAction player
        + scoreForOutcome (outcome actions)

    parsePlayer :: String -> Action
    parsePlayer s = case s of
      "X" -> Rock
      "Y" -> Paper
      "Z" -> Scissors
      _ -> error "Not a valid action"

part2 :: String -> Int
part2 contents =
  foldl' parse 0 $
    parseRound parseOpponent parsePlayer
      <$> lines contents
  where
    parse :: Int -> (Action, Outcome) -> Int
    parse points result =
      let playerAction = actionForResult result
          actions = (fst result, playerAction)
       in points
            + scoreForAction playerAction
            + scoreForOutcome (outcome actions)

    parsePlayer :: String -> Outcome
    parsePlayer s = case s of
      "X" -> Lose
      "Y" -> Draw
      "Z" -> Win
      _ -> error "Not a valid action"

    actionForResult :: (Action, Outcome) -> Action
    actionForResult = \case
      (Rock, Lose) -> Scissors
      (Rock, Draw) -> Rock
      (Rock, Win) -> Paper
      (Paper, Lose) -> Rock
      (Paper, Draw) -> Paper
      (Paper, Win) -> Scissors
      (Scissors, Lose) -> Paper
      (Scissors, Draw) -> Scissors
      (Scissors, Win) -> Rock

parseRound :: (String -> opponent) -> (String -> player) -> String -> (opponent, player)
parseRound po pp = bimap po pp . splitAt 2

data Action
  = Rock
  | Paper
  | Scissors

parseOpponent :: String -> Action
parseOpponent s = case s of
  "A " -> Rock
  "B " -> Paper
  "C " -> Scissors
  _ -> error "Not a valid action"

scoreForAction :: Action -> Int
scoreForAction = \case
  Rock -> 1
  Paper -> 2
  Scissors -> 3

data Outcome
  = Lose
  | Draw
  | Win

{-
|       | **R** | **P** | **S** |
| ----- | ----- | ----- | ----- |
| **R** | D     | L     | W     |
| **P** | W     | D     | L     |
| **S** | L     | W     | D     |
 -}
outcome :: (Action, Action) -> Outcome
outcome = \case
  (Rock, Scissors) -> Lose
  (Scissors, Paper) -> Lose
  (Paper, Rock) -> Lose
  (Rock, Rock) -> Draw
  (Paper, Paper) -> Draw
  (Scissors, Scissors) -> Draw
  (Rock, Paper) -> Win
  (Paper, Scissors) -> Win
  (Scissors, Rock) -> Win

scoreForOutcome :: Outcome -> Int
scoreForOutcome = \case
  Lose -> 0
  Draw -> 3
  Win -> 6
