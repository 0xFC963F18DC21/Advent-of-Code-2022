{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module AoC2022 where

import Control.Monad
import Control.Lens
import Data.Array ( Array )
import qualified Data.Array as Ar
import Data.Maybe ( fromMaybe )
import ScannerGeneric
import System.IO ( hFlush, stdout )
import Text.Read ( readMaybe )
import qualified Text.Regex.Base as RegEx
import qualified Text.Regex.PCRE as PCRE

-- Preliminaries and helpers.
getInput :: Int -> IO [String]
getInput day
  | day < 1 || day > 25 = error "Invalid day!"
  | otherwise           = lines <$> readFile ("inputs/day" ++ (show day ++ ".txt"))

prompt :: String -> IO String
prompt msg = putStr msg >> hFlush stdout >> getLine

readDef :: Read a => a -> String -> a
readDef def = fromMaybe def . readMaybe

-- Main function.
main :: IO ()
main = do
  -- Get the day and part number.
  day  <- prompt "Input the day number: "
  part <- prompt "Input the part number: "

  let dn = readDef 0 day :: Int
      dp = readDef 0 part :: Int

  -- Run that part.
  runPart dn dp

-- Day function declarations.
type Day = Int -> IO ()

runPart :: Int -> Int -> IO ()
runPart day p 
  | day < 1 || day > 25 = error "Invalid day!"
  | p /= 1 && p /= 2    = error "Invalid part!"
  | otherwise           = (days !! (day - 1)) p

days :: [Day]
days =
  [  day1,  day2,  day3,  day4,  day5
  ,  day6,  day7,  day8,  day9, day10
  , day11, day12, day13, day14, day15
  , day16, day18, day17, day19, day20
  , day21, day22, day23, day24, day25
  ]

todo :: IO ()
todo = putStrLn "TODO"

-- Day 1.
day1 :: Day
day1 _ = getInput 1 >> todo

-- Day 2.
day2 :: Day
day2 _ = getInput 2 >> todo

-- Day 3.
day3 :: Day
day3 _ = getInput 3 >> todo

-- Day 4.
day4 :: Day
day4 _ = getInput 4 >> todo

-- Day 5.
day5 :: Day
day5 _ = getInput 5 >> todo

-- Day 6.
day6 :: Day
day6 _ = getInput 6 >> todo

-- Day 7.
day7 :: Day
day7 _ = getInput 7 >> todo

-- Day 8.
day8 :: Day
day8 _ = getInput 8 >> todo

-- Day 9.
day9 :: Day
day9 _ = getInput 9 >> todo

-- Day 10.
day10 :: Day
day10 _ = getInput 10 >> todo

-- Day 11.
day11 :: Day
day11 _ = getInput 11 >> todo

-- Day 12.
day12 :: Day
day12 _ = getInput 12 >> todo

-- Day 13.
day13 :: Day
day13 _ = getInput 13 >> todo

-- Day 14.
day14 :: Day
day14 _ = getInput 14 >> todo

-- Day 15.
day15 :: Day
day15 _ = getInput 15 >> todo

-- Day 16.
day16 :: Day
day16 _ = getInput 16 >> todo

-- Day 17.
day17 :: Day
day17 _ = getInput 17 >> todo

-- Day 18.
day18 :: Day
day18 _ = getInput 18 >> todo

-- Day 19.
day19 :: Day
day19 _ = getInput 19 >> todo

-- Day 20.
day20 :: Day
day20 _ = getInput 20 >> todo

-- Day 21.
day21 :: Day
day21 _ = getInput 21 >> todo

-- Day 22.
day22 :: Day
day22 _ = getInput 22 >> todo

-- Day 23.
day23 :: Day
day23 _ = getInput 23 >> todo

-- Day 24.
day24 :: Day
day24 _ = getInput 24 >> todo

-- Day 25.
day25 :: Day
day25 _ = getInput 25 >> todo
