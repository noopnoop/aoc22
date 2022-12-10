module Day4 (solve) where
import Text.Parsec.ByteString.Lazy (Parser, parseFromFile)
import Text.Parsec (many1)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (sepBy)
import Text.Parsec (char)
import Text.Parsec (sepEndBy)

type Range   = (Int,Int)
type Pair    = (Range,Range)
type Dataset = [Pair]

tuplify :: [a] -> (a,a)
tuplify (a:b:_) = (a,b)
tuplify _        = error "tuplify error: list has less than 2 elements"

parseInt     :: Parser Int
parseRange   :: Parser Range
parsePair    :: Parser Pair
parseDataset :: Parser Dataset
parseInt     =  read <$> many1 digit
parseRange   =  tuplify <$> parseInt `sepBy` char '-'
parsePair    =  tuplify <$> parseRange `sepBy` char ','
parseDataset =  parsePair `sepEndBy` char '\n'

fullyContains :: Pair -> Bool
fullyContains (a,b) = leftHasRight (a,b) || leftHasRight (b,a)
  where leftHasRight ((p,q),(x,y)) = p <= x && q >= y

overlaps :: Pair -> Bool
overlaps (a,b) = overlapsOnRight (a,b) || overlapsOnRight (b,a)
  where overlapsOnRight ((_,q),(x,y)) = q >= x && q <= y

countTrue :: [Bool] -> Int
countTrue []     = 0
countTrue (x:xs) = countTrue xs + if x then 1 else 0

doMath :: Dataset -> Int
doMath = countTrue . map overlaps

solve :: IO ()
solve = do
  input <- parseFromFile parseDataset "input.txt"
  case input of
    Left err   -> print err
    Right good -> print $ doMath good