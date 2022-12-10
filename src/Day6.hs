module Day6 (solve) where
import Data.List (nub)

allUnique :: Eq a => [a] -> Bool
allUnique xs = nub xs == xs

-- >>> firstMarker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
-- 26
firstMarker :: String -> Int
firstMarker = firstMarker' 14

firstMarker' :: Int -> String -> Int
firstMarker' _ []     = error "no marker"
firstMarker' n (x:xs) =
  if allUnique $ take 14 (x:xs)
      then n
      else firstMarker' (n+1) xs

solve :: IO ()
solve = do
  input <- readFile "input.txt"
  print $ firstMarker input
