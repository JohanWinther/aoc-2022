#!/usr/bin/env runhaskell

import Data.Bifunctor (Bifunctor (first, second))
import Data.List (foldl', intercalate, intersperse)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 =
  ( \fs ->
      let allDirs = dirs fs
       in foldl'
            ( \total path ->
                let s = size path fs
                 in if s < 100000
                      then total + s
                      else total
            )
            0
            allDirs
  )
    . parseFileSystem

part2 :: String -> Int
part2 =
  ( \fs ->
      let allDirs = dirs fs
       in foldl'
            ( \smallest path ->
                let s = size path fs
                 in if deleteFileIfAtLeast fs < s && s < smallest
                      then s
                      else smallest
            )
            neededUnusedSpace
            allDirs
  )
    . parseFileSystem
  where
    totalSpace = 70000000
    unusedSpace fs = totalSpace - size root fs
    neededUnusedSpace = 30000000
    deleteFileIfAtLeast fs = neededUnusedSpace - unusedSpace fs

type FileSystem = Map Path Entity

parseFileSystem :: String -> FileSystem
parseFileSystem =
  snd
    . foldl' parse (mempty, Map.fromList [(root, Directory [])])
    . lines
  where
    parse :: (Path, FileSystem) -> String -> (Path, FileSystem)
    parse cur@(path@(Path cwd), fs) line = case line of
      [] ->
        cur
      "$ ls" ->
        cur
      "$ cd .." ->
        (Path (tail cwd), fs)
      '$' : ' ' : 'c' : 'd' : ' ' : dir ->
        (Path (dir : cwd), fs)
      'd' : 'i' : 'r' : ' ' : dir ->
        (path, mkdir dir path fs)
      file ->
        (path, touch (first read (splitOn ' ' file)) path fs)

    splitOn :: Char -> String -> (String, String)
    splitOn c = second (drop 1) . span (c /=)

size :: Path -> FileSystem -> Int
size path@(Path p) fs = case Map.lookup path fs of
  Just (File s) ->
    s
  Just (Directory c) ->
    foldl' (\s f -> s + size (Path (f : p)) fs) 0 c
  Nothing ->
    0

dirs :: FileSystem -> [Path]
dirs =
  Map.foldrWithKey' (\path entity paths -> path : paths) []
    . Map.filter isDir

mkdir :: String -> Path -> FileSystem -> FileSystem
mkdir name path@(Path p) =
  Map.adjust (addRef name) path
    . Map.insert (Path (name : p)) (Directory [])

touch :: (Int, String) -> Path -> FileSystem -> FileSystem
touch (size, name) path@(Path p) =
  Map.adjust (addRef name) path
    . Map.insert (Path (name : p)) (File size)

newtype Path = Path [String]
  deriving (Eq, Ord, Semigroup, Monoid)

root :: Path
root = Path ["/"]

instance Show Path where
  show :: Path -> String
  show (Path []) = "Path \"\""
  show (Path [x]) = "Path \"/\""
  show (Path p) = "Path \"" <> intercalate "/" ("" : tail (reverse p)) <> "\""

data Entity
  = Directory Contents
  | File Int
  deriving (Show)

type Contents = [String]

isDir :: Entity -> Bool
isDir (Directory _) = True
isDir (File _) = False

addRef :: String -> Entity -> Entity
addRef d (Directory c) = Directory (d : c)
addRef d x = x
