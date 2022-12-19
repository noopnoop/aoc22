{-# LANGUAGE FlexibleInstances #-}

module Day13 (solve) where
import Text.Parsec.ByteString.Lazy (Parser, parseFromFile)
import Text.Parsec (between, many1, digit, char, (<|>), sepEndBy, sepBy, string, newline)
import Data.List (sort)
import Debug.Trace (traceShowId)

data Chunk = INT Int | CHUNKS [Chunk]
  deriving (Eq, Show)

type Pair = ([Chunk],[Chunk])

instance Ord Chunk where
  compare (INT p) (INT q)       = compare p q
  compare (CHUNKS p) (CHUNKS q) = compare p q
  compare (INT p) (CHUNKS q)    = compare (CHUNKS [INT p]) (CHUNKS q)
  compare (CHUNKS p) (INT q)    = compare (CHUNKS p) (CHUNKS [INT q])

instance {-# OVERLAPPING #-} Ord [Chunk] where
  compare [] [] = EQ
  compare _ []  = GT
  compare [] _  = LT
  compare (x:xs) (y:ys) =
    case compare x y of
       EQ -> compare xs ys
       LT -> LT
       GT -> GT

inOrder :: Pair -> Bool
inOrder = uncurry (<)

doMathOld :: [Pair] -> Int
doMathOld pairs = sum $ map snd $ filter (inOrder . fst) $ zip pairs [1..]

dividers :: [[Chunk]]
dividers = 
  [ [CHUNKS [INT 2]]
  , [CHUNKS [INT 6]]
  ]

isDivider :: [Chunk] -> Bool
isDivider = flip elem dividers

doMath :: [[Chunk]] -> Int
doMath = product . map fst . filter (isDivider . snd) . zip [1..] . sort . (++) dividers

untuple :: Pair -> [[Chunk]]
untuple (x,y) = [x,y]

parseInt        :: Parser Int
parsePacket     :: Parser [Chunk]
parsePair       :: Parser Pair
parseDatasetOld :: Parser [Pair]
parseDataset    :: Parser [[Chunk]]

parseInt    = read <$> many1 digit
parsePacket = between (char '[') (char ']') 
            $ ((INT <$> parseInt) <|> (CHUNKS <$> parsePacket)) 
              `sepEndBy` char ','
parsePair   = do
  first  <- parsePacket
  _      <- newline
  second <- parsePacket
  return (first, second)
parseDatasetOld = parsePair `sepBy` string "\n\n"
parseDataset = concat . map untuple <$> parseDatasetOld

solve :: IO ()
solve = do
  dataset <- parseFromFile parseDataset "input.txt"
  case dataset of
    Left err   -> print err
    Right good -> print $ doMath good