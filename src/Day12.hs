module Day12 (solve) where
import qualified Data.Map as M
import qualified Data.Array as A
import Debug.Trace (traceShowM, traceShowId, traceShow)
import Data.Foldable (find)
import Data.List (unfoldr)
import Algebra.Graph.AdjacencyMap

type Square = (Int,Int)
type HMap   = A.Array Square Char
type Paths  = M.Map Square Char

letters :: [Char]
letters = ['a'..'z']
vals :: [Int]
vals = [1..26]
pairs :: M.Map Char Int
pairs = M.fromList $ zip letters vals ++ [('S',1),('E',26)]
elevation :: Char -> Int
elevation c =
  case M.lookup c pairs of
    Nothing -> error "priority error: char outside alphabet"
    Just n  -> n

canReach :: Char -> Char -> Bool
canReach p q = elevation q - elevation p <= 1

findKey :: A.Ix i => (a -> Bool) -> A.Array i a -> i
findKey fn arr = head $ map fst $ filter (fn . snd) $ A.assocs arr

reachedEnd :: Paths -> Bool
reachedEnd paths =
  case find (=='a') paths of
    Nothing -> False
    Just _  -> True

(+++) :: Square -> Square -> Square
(+++) (x,y) (a,b) = (x+a,y+b)

adjacents :: Square -> HMap -> Paths
adjacents sq arr = makeMap $ filter (\x -> inBounds x && goodHeight x) adjacentSquares
  where adjacentSquares = [sq, sq +++ (1,0), sq +++ (-1,0), sq +++ (0,1), sq +++ (0,-1) ]
        inBounds        = A.inRange $ A.bounds arr
        goodHeight x    = canReach (arr A.! x) (arr A.! sq)
        makeMap sqs     = M.fromList $ zip sqs $ map (arr A.!) sqs

bfs :: [Paths] -> HMap -> [Paths]
bfs visited hmap =
  if reachedEnd doRep
    then visited ++ [doRep]
    else if doRep == last visited
      then error "stuck"
      else bfs (visited ++ [doRep]) hmap
  where doRep = foldr1 M.union $ map (`adjacents` hmap) $ M.keys $ last visited

solve :: IO ()
solve = do
  input     <- lines <$> readFile "input.txt"
  let bounds = (length input, length $ head input)
  let arr    = A.listArray ((1,1), bounds) (concat input)
  let start  = findKey (=='E') arr
  let search = bfs [M.singleton start 'E'] arr
  print $ length $ tail search