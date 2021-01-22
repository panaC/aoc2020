module Main where

import qualified Data.IntSet as IntSet
import qualified Data.List.Split as Split

import Text.Regex.TDFA

numberZipped :: [a] -> [(a,a)]
numberZipped [] = []
numberZipped (x:[]) = []
numberZipped (x:y:xs) = [(x,y)] ++ numberZipped xs

nbSet :: [Char] -> [Int]
nbSet str =
  let numberStr = getAllTextMatches ( str =~ "[0-9]+" )  :: [String]
  in map (\x -> read x :: Int) numberStr


rulesSet :: [Char] -> IntSet.IntSet
rulesSet rules =
  let number = nbSet rules
      numberZip = numberZipped number
      numberRange =  map (\(a, b) -> [a..b]) numberZip
      numberSet = map IntSet.fromList numberRange
  in IntSet.unions numberSet

main :: IO ()
main = do
  contents <- getContents
  let lnsSplited = Split.splitOn "\n\n" contents
  let rules = rulesSet $ head lnsSplited
  let nearbyTickets = nbSet $ lnsSplited !! 2
  let wrongRules = filter (\x -> IntSet.notMember x rules) nearbyTickets

  -- print $ filter odd $ nbSet $ lnsSplited !! 0
  -- print $ numberZipped . nbSet $ lnsSplited !! 0
  -- print wrongRules
  print $ sum wrongRules
