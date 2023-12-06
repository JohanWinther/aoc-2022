#!/usr/bin/env runhaskell

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Debug.Trace as Debug

main :: IO ()
main = do
  input <- readFile "example.txt"
  -- input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

-- part1 :: String -> Grid
part1 =
  (\grid -> (grid, availableMoves (Position 1 1) grid))
    . fromString
    . lines

part2 :: String -> Int
part2 _ =
  0

newtype Grid = Grid (Map Position Char)

data Position = Position Int Int
  deriving (Eq)

instance Show Position where
  show :: Position -> String
  show (Position x y) = "(" <> show x <> "," <> show y <> ")"

instance Ord Position where
  compare :: Position -> Position -> Ordering
  compare (Position x1 y1) (Position x2 y2)
    | y1 < y2 = LT
    | y1 == y2 && x1 < x2 = LT
    | y1 == y2 && x1 == x2 = EQ
    | y1 == y2 && x1 > x2 = GT
    | y1 > y2 = GT

fromString :: [String] -> Grid
fromString rows =
  Grid $
    Map.fromList $
      foldl
        ( \positions (y, row) ->
            foldl
              (\positions' (x, char) -> (Position x y, char) : positions')
              mempty
              (enumerate row)
              <> positions
        )
        mempty
        (enumerate rows)
  where
    enumerate :: [a] -> [(Int, a)]
    enumerate = zip [0 ..]

availableMoves :: Position -> Grid -> [Position]
availableMoves p@(Position x y) (Grid cells) =
  let c = elevationChar $ cells Map.! p
      (Position xMax yMax, _) = Map.findMax cells
   in filter
        ( \p'@(Position x' y') ->
            let c' = elevationChar $ cells Map.! p'
             in (0 <= x' && x' <= xMax)
                  && (0 <= y' && y' <= yMax)
                  && fromEnum c' <= fromEnum c + 1
        )
        [ Position x (y - 1),
          Position (x - 1) y,
          Position (x + 1) y,
          Position x (y + 1)
        ]
  where
    elevationChar c
      | c == 'S' = 'a'
      | c == 'E' = 'z'
      | otherwise = c

instance Show Grid where
  show (Grid cells) =
    let (Position xMax yMax, _) = Map.findMax cells
        row y = (\x -> cells Map.! Position x y) <$> [0 .. xMax]
     in show (xMax + 1) <> "x" <> show (yMax + 1) <> "\n" <> (unlines . fmap row $ [0 .. yMax])
