module Day2 (solve) where
import Text.Parsec (space, parse)
import Text.Parsec.Char (char)
import Text.Parsec (upper, newline)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec.ByteString.Lazy (Parser, parseFromFile)

data Roshambo = Rock | Paper | Scissors
  deriving (Eq, Ord, Show, Enum)

scoreShape :: Roshambo -> Int
scoreShape shape =
  case shape of
    Rock     -> 1
    Paper    -> 2
    Scissors -> 3

-- >>> scoreRoshambo Scissors Scissors
-- 6
scoreRoshambo :: Roshambo -> Roshambo -> Int
scoreRoshambo yours mine =
  case (yours,mine) of
    (Rock, Rock) -> 0 + 3
    (Rock, Paper) -> 3 + 1
    (Rock, Scissors) -> 6 + 2
    (Paper, Rock) -> 0 + 1
    (Paper, Paper) -> 3 + 2
    (Paper, Scissors) -> 6 + 3
    (Scissors, Rock) -> 0 + 2
    (Scissors, Paper) -> 3 + 3
    (Scissors, Scissors) -> 6 + 1

type Game = (Roshambo, Roshambo)
type Dataset = [Game]

charToRoshambo :: Char -> Roshambo
charToRoshambo c =
  case c of
    'A' -> Rock
    'B' -> Paper
    'C' -> Scissors
    'X' -> Rock
    'Y' -> Paper
    'Z' -> Scissors

-- >>> parse datasetParser "" "B Y\nC A"
-- Right [(Paper,Paper),(Scissors,Rock)]
roshamboParser :: Parser Roshambo
gameParser     :: Parser Game
datasetParser  :: Parser Dataset
roshamboParser = charToRoshambo <$> upper
gameParser     = do
  yours <- roshamboParser
  space
  mine <- roshamboParser
  return (yours,mine)
datasetParser  = gameParser `sepBy` newline

doMath :: Dataset -> Int
doMath = sum . map (uncurry scoreRoshambo)

solve :: IO ()
solve = do
  dataset <- parseFromFile datasetParser "input.txt"
  case dataset of
    Left  err   -> print err
    Right good  -> print $ doMath good
