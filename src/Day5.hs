module Day5 (solve) where

import Control.Monad.State (State, gets, modify, void, replicateM_, MonadState (get), replicateM, evalState)
import Text.Parsec.ByteString.Lazy (Parser, parseFromFile)
import Text.Parsec (many1, digit, string, sepEndBy)
import Text.Parsec.Char (char)

type Stacks = [String]
type StackAction a = State Stacks a

-- index function for 1-indexed lists. sorry
(!!!) :: Int -> [a] -> a
(!!!) i as = as !! (i - 1)

-- copied from https://hackage.haskell.org/package/ilist-0.4.0.1/docs/Data-List-Index.html#v:setAt
-- and then modified to do something different! dsaf
-- and then changed to be 1-indexed! grahhh
applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt i fn ls
  | i < 1 = ls
  | otherwise = go i ls
  where
    go 1 (x:xs) = fn x : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

setAt :: Int -> a -> [a] -> [a]
setAt i x = applyAt i $ const x

dropOne :: Int -> StackAction Char
dropOne from = do
  dropped <- gets $ head . (!!!) from
  modify  $  applyAt from tail
  return dropped

addOne :: Int -> Char -> StackAction Stacks
addOne to x = do
  modify $ applyAt to (x:)
  get

moveOnce :: Int -> Int -> StackAction Stacks
moveOnce from to = 
  dropOne from >>= addOne to

move :: Int -> Int -> Int -> StackAction Stacks
move times from to
  = last <$> replicateM times (moveOnce from to)

moveMany :: Int -> Int -> Int -> StackAction Stacks
moveMany amt from to = do
  taken <- gets $ take amt . (!!!) from
  modify $ applyAt from $ drop amt
  modify $ applyAt to $ (++) taken
  get

getTops :: Stacks -> String
getTops = map head

-- >>> doMath ["NZ","DCM","P"] [move 1 2 1, move 3 1 3, move 2 2 1, move 1 1 2]
-- "CMZ"
doMath :: Stacks -> [StackAction Stacks] -> String
doMath xs fns = getTops $ last $ evalState (sequence fns) xs

parseInt :: Parser Int
parseLine :: Parser (StackAction Stacks)
parseActions :: Parser [StackAction Stacks]
parseInt = read <$> many1 digit
parseLine = do
  string "move "
  times <- parseInt
  string " from "
  from <- parseInt
  string " to "
  to <- parseInt
  return $ moveMany times from to
parseActions = parseLine `sepEndBy` char '\n'

-- probably easier to just do this than write a parser for the crate section lol
initialState :: Stacks
initialState = 
  [ "NVCS"
  , "SNHJMZ"
  , "DNJGTCM"
  , "MRWJFDT"
  , "HFP"
  , "JHZTC"
  , "ZLSFQRPD"
  , "WPFDHLSC"
  , "ZGNFPMSD"
  ]

solve :: IO ()
solve = do
  input <- parseFromFile parseActions "input.txt"
  case input of
    Left  err  -> print err
    Right good -> print $ doMath initialState good
