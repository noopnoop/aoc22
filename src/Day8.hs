module Day8 (solve) where
import Text.Parsec.ByteString.Lazy (Parser, parseFromFile)
import Text.Parsec (digit)
import Text.Parsec.Combinator (sepEndBy)
import Text.Parsec.Char (char)
import Text.ParserCombinators.Parsec (many)
import Data.Char (digitToInt)
import Data.List (transpose, zipWith4)

-- >>> visibleFromLeft' (-1) [3,0,3,7,3]
-- [True,False,False,True,False]
visibleFromLeft' :: Ord a => a -> [a] -> [Bool]
visibleFromLeft' _ []     = []
visibleFromLeft' n (x:xs) = 
  if x > n
    then True  : visibleFromLeft' x xs
    else False : visibleFromLeft' n xs

visibleFromLeft :: [[Int]] -> [[Bool]]
visibleFromLeft = map $ visibleFromLeft' (-1)

visibleFromRight :: [[Int]] -> [[Bool]]
visibleFromRight = map reverse . visibleFromLeft . map reverse

visibleFromTop :: [[Int]] -> [[Bool]]
visibleFromTop = transpose . visibleFromLeft . transpose

visibleFromBottom :: [[Int]] -> [[Bool]]
visibleFromBottom = transpose . visibleFromRight . transpose

fourOrs :: Bool -> Bool -> Bool -> Bool -> Bool
fourOrs a b c d = a || b || c || d

visibleFromSomewhere :: [[Int]] -> [[Bool]]
visibleFromSomewhere xs = zipWith4 (zipWith4 fourOrs) fromLeft fromRight fromTop fromBottom
  where fromLeft   = visibleFromLeft xs
        fromRight  = visibleFromRight xs
        fromTop    = visibleFromTop xs
        fromBottom = visibleFromBottom xs

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

specialLessThan :: Int -> [Int] -> Int
specialLessThan n [] = 0
specialLessThan n (x:xs) = 1 +
  if n > x
    then specialLessThan n xs
    else 0

rightScore :: (Int,Int) -> [[Int]] -> Int
rightScore (x,y) trees = specialLessThan height onRight
  where height  = (trees !! y) !! x
        onRight = drop (x+1) $ trees !! y

leftScore :: (Int,Int) -> [[Int]] -> Int
leftScore (x,y) trees = rightScore (x',y) $ map reverse trees
  where x' = length (head trees) - x - 1

downScore :: (Int,Int) -> [[Int]] -> Int
downScore (x,y) trees = rightScore (y,x) $ transpose trees

upScore :: (Int,Int) -> [[Int]] -> Int
upScore (x,y) trees = leftScore (y,x) $ transpose trees

score :: (Int,Int) -> [[Int]] -> Int
score (x,y) trees = rightScore (x,y) trees * leftScore (x,y) trees * downScore (x,y) trees * upScore (x,y) trees

doMath :: [[Int]] -> Int
doMath trees = maximum scores
  where scores  = map (`score` trees) indices
        indices = [(x,y) | x <- [0..width], y <- [0..height]]
        width   = length (head trees) - 1
        height  = length trees - 1

doMathOld :: [[Int]] -> Int
doMathOld = sum . map (sum . map boolToInt) . visibleFromSomewhere

parseInt     :: Parser Int
parseDataset :: Parser [[Int]]
parseInt     = digitToInt <$> digit
parseDataset = many parseInt `sepEndBy` char '\n'

solve :: IO ()
solve = do
  dataset <- parseFromFile parseDataset "input.txt"
  case dataset of
    Left  err  -> print err
    Right good -> print $ doMath good