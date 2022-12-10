module Day3 (solve) where
import Data.List (find, intersect)
import qualified Data.Map as M

halve :: [a] -> ([a],[a])
halve as = splitAt (length as `div` 2) as

findMatch :: Eq a => [a] -> [a] -> a
findMatch []     _  = error "findMatch error: no match found"
findMatch (x:xs) ys =
  case find (==x) ys of
    Just y  -> y
    Nothing -> findMatch xs ys

findMatches :: Eq a => [[a]] -> [a]
findMatches = foldr1 intersect

letters :: [Char]
letters = ['a'..'z'] ++ ['A'..'Z']
vals :: [Int]
vals = [1..52]
pairs :: M.Map Char Int
pairs = M.fromList $ zip letters vals
priority :: Char -> Int
priority c =
  case M.lookup c pairs of
    Nothing -> error "priority error: char outside alphabet"
    Just n  -> n

-- >>> makeGroups ["a","b","c"]
makeGroups :: [String] -> [[String]]
makeGroups []         = []
makeGroups (a:b:c:xs) = [a,b,c] : makeGroups xs
makeGroups _          = error "elf overflow"

doMath :: [[String]] -> Int
doMath = sum . map (priority . head . findMatches)

solve :: IO ()
solve = do
  input         <- readFile "input.txt"
  let formatted =  makeGroups $ lines input
  print         $  doMath formatted
