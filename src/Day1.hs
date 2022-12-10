module Day1 (solve) where

import Text.Parsec (newline)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec (sepEndBy1)
import Text.Parsec (sepEndBy, parse)
import Data.List (sort)

type Calorie = Int
type Elf     = [Calorie]
type DataSet = [Elf]

-- >>> parse datasetParser "" "1111\n5555\n\n66\n\n2\n3\n\n00\n"
-- Right [[1111,5555],[66],[2,3],[0]]
calorieParser :: Parser Int
elfParser     :: Parser Elf
datasetParser :: Parser DataSet
calorieParser = read <$> many1 digit
elfParser     = calorieParser `sepEndBy1` newline
datasetParser = elfParser `sepEndBy` newline

doMath :: DataSet -> Calorie
doMath = sum . take 3 . reverse . sort . map sum

solve :: IO ()
solve = do
  dataset <- parseFromFile datasetParser "input.txt"
  case dataset of
    Left  err   -> print err
    Right good  -> print $ doMath good