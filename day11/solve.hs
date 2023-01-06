#!/usr/bin/env runhaskell

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (iterate', sort)
import qualified Debug.Trace as Debug

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

example :: (Rules, State)
example =
  ( IntMap.fromList $
      enumerate
        [ Monkey (* 19) 23 (2, 3),
          Monkey (+ 6) 19 (2, 0),
          Monkey (^ 2) 13 (1, 3),
          Monkey (+ 3) 17 (0, 1)
        ],
    IntMap.fromList $
      enumerate
        [ ([79, 98], 0),
          ([54, 65, 75, 74], 0),
          ([79, 60, 97], 0),
          ([74], 0)
        ]
  )

initial :: (Rules, State)
initial =
  ( IntMap.fromList $
      enumerate
        [ Monkey (* 7) 19 (6, 7),
          Monkey (* 11) 3 (3, 5),
          Monkey (+ 8) 13 (0, 6),
          Monkey (+ 7) 7 (2, 4),
          Monkey (+ 5) 5 (2, 0),
          Monkey (+ 4) 11 (4, 3),
          Monkey (^ 2) 17 (7, 1),
          Monkey (+ 3) 2 (5, 1)
        ],
    IntMap.fromList $
      enumerate
        [ ([85, 77, 77], 0),
          ([80, 99], 0),
          ([74, 60, 74, 63, 86, 92, 80], 0),
          ([71, 58, 93, 65, 80, 68, 54, 71], 0),
          ([97, 56, 79, 65, 58], 0),
          ([77], 0),
          ([99, 90, 84, 50], 0),
          ([50, 66, 61, 92, 64, 78], 0)
        ]
  )

part1 :: String -> Int
part1 _ =
  (product . take 2 . reverse . sort . IntMap.elems . fmap snd)
    . (last . take (20 + 1) . iterate' (playRound rules))
    $ state
  where
    (rules, state) = initial

playRound :: Rules -> State -> State
playRound rules state =
  foldl
    (\stateAfterTurn i -> playTurn i (rules IntMap.! i) stateAfterTurn)
    state
    indices
  where
    indices = [0 .. IntMap.size rules - 1]
    playTurn :: MonkeyId -> Monkey -> State -> State
    playTurn i rule state' =
      let (items, inspections) = state' IntMap.! i
          updateCurrentMonkey = IntMap.insert i ([], inspections + length items)
          thrownItems = applyRule rule items
       in updateThrownItems thrownItems
            . updateCurrentMonkey
            $ state'

    updateThrownItems :: [(MonkeyId, Item)] -> State -> State
    updateThrownItems =
      flip $
        foldl
          ( \intMap (idx, item) ->
              let (items, insp) = intMap IntMap.! idx
               in IntMap.insert idx (items ++ [item], insp) intMap
          )

    applyRule :: Monkey -> [Item] -> [(MonkeyId, Item)]
    applyRule (Monkey op d a) =
      fmap
        ( \item ->
            let newLevel = op item `quot` 3
             in if newLevel `mod` d == 0
                  then (fst a, newLevel)
                  else (snd a, newLevel)
        )

-- part2 :: String -> Int
part2 _ =
  ( product
      . take 2
      . reverse
      . sort
      . IntMap.elems
      . fmap snd
  )
    . (last . take (10000 + 1) . iterate' (playRoundMod rules))
    $ state
  where
    (rules, state) = initial

playRoundMod :: Rules -> State -> State
playRoundMod rules state =
  foldl
    (\stateAfterTurn i -> playTurn i (rules IntMap.! i) stateAfterTurn)
    state
    indices
  where
    indices = [0 .. IntMap.size rules - 1]
    playTurn :: MonkeyId -> Monkey -> State -> State
    playTurn i rule state' =
      let (items, inspections) = state' IntMap.! i
          updateCurrentMonkey = IntMap.insert i ([], inspections + length items)
          thrownItems = applyRule rule items
       in updateThrownItems thrownItems
            . updateCurrentMonkey
            $ state'

    updateThrownItems :: [(MonkeyId, Item)] -> State -> State
    updateThrownItems =
      flip $
        foldl
          ( \intMap (idx, item) ->
              let (items, insp) = intMap IntMap.! idx
               in IntMap.insert idx (items ++ [item], insp) intMap
          )

    applyRule :: Monkey -> [Item] -> [(MonkeyId, Item)]
    applyRule (Monkey op d a) =
      fmap
        ( \item ->
            let newLevel = op item `mod` p
             in if newLevel `mod` d == 0
                  then (fst a, newLevel)
                  else (snd a, newLevel)
        )
      where
        p = product . fmap (\(Monkey _ d _) -> d) $ rules

type State = IntMap ([Item], Inspections)

type Inspections = Int

type Rules = IntMap Monkey

data Monkey = Monkey Operation DivisibleBy Action

instance Show Monkey where
  show (Monkey _ d (t, f)) = "Monkey (% " <> show d <> " ? " <> show t <> " : " <> show f <> ")"

type MonkeyId = Int

type Operation = Item -> Item

type DivisibleBy = Int

type Action = (MonkeyId, MonkeyId)

type Item = Int

-- Utils
enumerate :: [a] -> [(Int, a)]
enumerate = zip (iterate succ 0)
