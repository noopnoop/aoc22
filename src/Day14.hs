{-# LANGUAGE TupleSections #-}

module Day14 (solve) where
import qualified Data.Array as A
import Control.Monad.State.Lazy (State, evalState, get, modify)
import Text.Parsec.ByteString.Lazy (Parser, parseFromFile)
import Text.Parsec (many1, digit, sepBy, char, string, sepEndBy)

type Point = (Int, Int)
type Path  = [Point]
type Grid  = A.Array Point Bool

bounds :: (Point,Point)
bounds = (,) (-2000,0) (2000,200)
source :: Point
source = (500,0)

initialGrid :: Grid
initialGrid = A.listArray bounds $ repeat False

intsBetween :: Int -> Int -> [Int]
intsBetween x y
  | x < y     = [x..y]
  | y < x     = [y..x]
  | otherwise = repeat x -- trust me

-- >>> pointsBetween (0,5) (0,0)
-- [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5)]
pointsBetween :: Point -> Point -> [Point]
pointsBetween (x1,y1) (x2,y2) =
  zip (intsBetween x1 x2) (intsBetween y1 y2)

addPoint :: Point -> Grid -> Grid
addPoint p g = g A.// [(p,True)]

addLine :: Point -> Point -> Grid -> Grid
addLine p q grid = grid A.// map (,True) (pointsBetween p q)

addPath :: Path -> Grid -> Grid
addPath []       grid = grid
addPath [_]      grid = grid
addPath (p:q:xs) grid = addPath (q:xs) (addLine p q grid)

applyAll :: a -> [a -> a] -> a
applyAll x []       = x
applyAll x (fn:fns) = applyAll (fn x) fns

addPaths :: [Path] -> Grid -> Grid
addPaths ps grid = applyAll grid $ map addPath ps

addFloor :: [Path] -> Grid -> Grid
addFloor ps = addLine (-2000,floorY) (2000, floorY)
  where floorY = (+2) $ maximum $ map snd $ concat ps

addBlocks :: [Path] -> Grid -> Grid
addBlocks ps = addFloor ps . addPaths ps

fall :: Point -> Grid -> Maybe Point
fall (x,y) grid
  | not $ A.inRange bounds (x,y+1) = Nothing
  | not $ grid A.! (x,y+1)         = Just (x,y+1)
  | not $ grid A.! (x-1,y+1)       = Just (x-1,y+1)
  | not $ grid A.! (x+1,y+1)       = Just (x+1,y+1)
  | otherwise                      = Just (x,y)

simulateDrop :: Point -> State Grid Bool
simulateDrop p = do
  grid <- get
  let q' = fall p grid
  case q' of
    Nothing -> error "fell out"
    Just q  -> if p == q
      then modify (addPoint p) >> 
        if p == source 
          then return True
          else return False
      else simulateDrop q

simulateDrops :: Int -> State Grid Int
simulateDrops n = do
  finished <- simulateDrop source
  if finished
    then return        $ n + 1
    else simulateDrops $ n + 1

doMath :: Grid -> Int
doMath grid = evalState (simulateDrops 0) grid  

tuplify :: [a] -> (a,a)
tuplify (a:b:_) = (a,b)
tuplify _        = error "tuplify error: list has less than 2 elements"

parseInt     :: Parser Int
parsePoint   :: Parser Point
parsePath    :: Parser Path
parseDataset :: Parser [Path]
parseInt     = read    <$> many1 digit
parsePoint   = tuplify <$> parseInt `sepBy` char ','
parsePath    = parsePoint `sepBy` string " -> "
parseDataset = parsePath `sepEndBy` char '\n'

solve :: IO ()
solve = do
  input <- parseFromFile parseDataset "input.txt"
  case input of
    Left err   -> print err
    Right good -> print $ doMath $ addBlocks good initialGrid