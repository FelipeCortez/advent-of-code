import System.IO
import Control.Monad
import Data.List
import qualified Data.Map as Map

main = do
  contents <- readFile "/Users/felipecortez/Dev/advent-of-code/2024/01.in"
  let numbers = map readInt . words $ contents
      indexedNumbers = zip numbers [0..]
      list1 = sort [x | (x,y) <- indexedNumbers, even y]
      list2 = sort [x | (x,y) <- indexedNumbers, odd  y]
  print (sum [abs (x - y) | (x,y) <- zip list1 list2])

  let list2Freqs = freqs list2
  print (sum [freq * x | (freq, x) <- [(Map.findWithDefault 0 x list2Freqs, x) | x <- list1]])

readInt :: String -> Int
readInt = read

freqs :: [Int] -> Map.Map Int Int
freqs = Map.fromListWith (+) . flip zip (repeat 1)
