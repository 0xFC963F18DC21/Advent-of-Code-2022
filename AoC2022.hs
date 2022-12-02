{-# LANGUAGE LambdaCase, OverloadedStrings, TemplateHaskell #-}

module AoC2022 where

import Control.Monad
import Control.Monad.Trans.Class ( lift )
import Control.Lens
import Data.Array ( Array )
import qualified Data.Array as Ar
import Data.ByteString.Lazy ( ByteString )
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as Ch
import qualified Data.List as L
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe, fromJust, isNothing )
import ScannerGeneric
import Stream
import System.IO ( hFlush, stdout )
import Text.ParserCombinators.ReadP
import Text.Read ( readMaybe, readEither )
import qualified Text.Regex.Base as RegEx
import Text.Regex.PCRE ( (=~), (=~~) )

-- Preliminaries and helpers.
getRawInput :: Int -> IO String
getRawInput day
  | day < 1 || day > 25 = error "Invalid day!"
  | otherwise           = readFile ("inputs/day" ++ (show day ++ ".txt"))

getInput :: Int -> IO [String]
getInput = (lines <$>) . getRawInput

getInputBytes :: Int -> IO ByteString
getInputBytes day
  | day < 1 || day > 25 = error "Invalid day!"
  | otherwise           = BS.readFile ("inputs/day" ++ (show day ++ ".txt"))

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
elfLine :: StrScannerT IO (Maybe Int)
elfLine = nextTokenMaybe >>= \ case
  Nothing -> return Nothing
  Just s  -> case readEither s of
    Left _  -> return Nothing
    Right i -> return (Just i)

elf :: StrScannerT IO [Int]
elf = fmap fromJust <$> scanManyUntil isNothing elfLine

day1 :: Day
day1 p = do
  elves <- getInput 1 >>= runOnWordsT (scanMany elf)
  print $ case p of
    1  -> maximum (sum <$> elves)
    ~2 -> sum $ take 3 $ L.sortBy (flip compare) $ fmap sum elves

-- Day 2.
data RPS
  = R | P | S
  deriving (Show, Eq, Ord, Enum, Bounded)

data RPSStatus
  = Win
  | Loss
  | Draw
  deriving (Show, Eq, Ord, Enum, Bounded)

rpsValue :: RPS -> Int
rpsValue R = 1
rpsValue P = 2
rpsValue S = 3

statusValue :: RPSStatus -> Int
statusValue Win  = 6
statusValue Draw = 3
statusValue Loss = 0

rpsMatch :: RPS -> RPS -> RPSStatus
rpsMatch R P = Win
rpsMatch P S = Win
rpsMatch S R = Win
rpsMatch x y
  | x == y    = Draw
  | otherwise = Loss

getScore1 :: RPS -> RPS -> Int
getScore1 opp you
  = let status = rpsMatch opp you
    in  rpsValue you + statusValue status

getScore2 :: RPS -> RPSStatus -> Int
getScore2 opp goal
  = let intended = getIntended opp goal
    in  rpsValue intended + statusValue goal
    where
      getIntended R Win  = P
      getIntended P Win  = S
      getIntended S Win  = R
      getIntended x Draw = x
      getIntended R Loss = S
      getIntended P Loss = R
      getIntended S Loss = P

scanRPS :: Monad m => StrScannerT m RPS
scanRPS = (\ case
  "A"  -> R
  "X"  -> R
  "B"  -> P
  "Y"  -> P
  "C"  -> S
  ~"Z" -> S) <$> nextToken

scanStatus :: Monad m => StrScannerT m RPSStatus
scanStatus = (\ case
  "X"  -> Loss
  "Y"  -> Draw
  ~"Z" -> Win) <$> nextToken

scanMatch1 :: Monad m => StrScannerT m (RPS, RPS)
scanMatch1 = (,) <$> scanRPS <*> scanRPS

scanMatch2 :: Monad m => StrScannerT m (RPS, RPSStatus)
scanMatch2 = (,) <$> scanRPS <*> scanStatus

day2 :: Day
day2 p = getRawInput 2 >>= runScannerT'_ (case p of
  1  -> do
    matches <- scanMany scanMatch1
    lift (print (L.foldl' (\ a x -> a + uncurry getScore1 x) 0 matches))
  ~2 -> do
    matches <- scanMany scanMatch2
    lift (print (L.foldl' (\ a x -> a + uncurry getScore2 x) 0 matches)))

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
