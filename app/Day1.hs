module Day1 where

import Data.List
import Data.Char 

part1 :: String -> IO ()
part1 shit = 
  let startedEl = (digitToInt (last shit), 0)
  in print (snd (foldl (getSum) startedEl (map digitToInt $ shit)))
  -- in print startedEl

getSum :: (Int, Int) -> Int -> (Int, Int)
getSum accum current = 
  if current == fst accum
      then (current, current + (snd accum))
      else (current, snd accum)
