module Day9 (solve) where
import Control.Monad.State.Lazy (State, MonadState (get), put, execState)
import Text.Parsec.ByteString.Lazy (Parser, parseFromFile)
import Text.Parsec (many1, digit, letter, space, char, sepEndBy)
import Data.List (nub)

type Point = (Int,Int)

moveTo :: Int -> Int -> Int
moveTo p q = 
  case compare p q of
    EQ -> p
    LT -> p + 1
    GT -> p - 1

tooFar :: Point -> Point -> Bool
tooFar (x,y) (a,b) = 
     abs (a-x) > 1
  || abs (b-y) > 1

(+++) :: Point -> Point -> Point
(+++) (x,y) (a,b) = (x+a,y+b)

moveTwo :: Point -> Point -> Point
moveTwo (x,y) (a,b) = 
  if tooFar (x,y) (a,b)
    then (moveTo x a, moveTo y b)
    else (x,y)

updateRope :: [Point] -> [Point]
updateRope (a:b:xs) = a : updateRope (moveTwo b a : xs)
updateRope [x]      = [x]
updateRope []       = []

data SimState = SimState
  { visited :: [Point]
  , rope    :: [Point]
  } deriving (Eq,Show)

type SimAction = State SimState ()

moveHead :: Point -> SimAction
moveHead dv = do
  (SimState oldVisited oldRope) <- get
  let (oldHead, oldTail) = (head oldRope, tail oldRope) -- blah
  let newHead = oldHead +++ dv
  let newRope = updateRope $ newHead : oldTail
  put $ SimState (last newRope : oldVisited) newRope

letterToMotion :: Char -> SimAction
letterToMotion 'L' = moveHead (-1,0)
letterToMotion 'R' = moveHead (1,0)
letterToMotion 'U' = moveHead (0,1)
letterToMotion 'D' = moveHead (0,-1)
letterToMotion _   = error "unexpected letter"

parseInt     :: Parser Int
parseMotion  :: Parser [SimAction]
parseMotions :: Parser [SimAction]
parseInt     = read <$> many1 digit
parseMotion  = do
  direction <- letterToMotion <$> letter
  _         <- space
  times     <- parseInt
  return     $ replicate times direction
parseMotions = concat <$> parseMotion `sepEndBy` char '\n'

doMath :: [SimAction] -> Int
doMath acns = length $ nub $ visited $ execState (sequence acns) $ SimState [] $ replicate 10 (0,0)

solve :: IO ()
solve = do
  dataset <- parseFromFile parseMotions "input.txt"
  case dataset of
    Left err   -> print err
    Right good -> print $ doMath good