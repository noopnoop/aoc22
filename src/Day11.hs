module Day11 (solve) where
import Control.Monad.State.Lazy
import Data.List (partition, sort)
import Debug.Trace (traceShowM)

data Monkey = Monkey Inventory Operation Test Partners Inspections

type Inventory = [Int]
type Operation = Int -> Int
type Test = Int -> Bool
type Partners = (Int,Int)
type Inspections = Int

type MState x = State [Monkey] x

giveToMonkey :: [Int] -> Monkey -> Monkey
giveToMonkey items (Monkey inv o t p i) = Monkey (inv ++ items) o t p i

countInspections :: Monkey -> Int
countInspections (Monkey _ _ _ _ inspections) = inspections

applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt i fn ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (x:xs) = fn x : xs
    go n (x:xs) = x : go (n-1) xs
    go _ []     = []

setAt :: Int -> a -> [a] -> [a]
setAt n a = applyAt n (const a)

doTurn :: Int -> MState ()
doTurn monkeyNum = do
  Monkey inv op test partners oldInspections <- gets (!! monkeyNum)
  let inspected = map (\x -> op x `mod` magicNumber) inv
  let newInspections = oldInspections + length inspected
  let (toFirst, toSecond) = partition test inspected
  modify $ setAt monkeyNum $ Monkey [] op test partners newInspections
  modify $ applyAt (fst partners) $ giveToMonkey toFirst
  modify $ applyAt (snd partners) $ giveToMonkey toSecond

divisibleBy :: Int -> Int -> Bool
divisibleBy n x = x `mod` n == 0

initialState :: [Monkey]
initialState =
  [ Monkey [54,89,94] (*7) (divisibleBy 17) (5,3) 0
  , Monkey [66,71] (+4) (divisibleBy 3) (0,3) 0
  , Monkey [76, 55, 80, 55, 55, 96, 78] (+2) (divisibleBy 5) (7,4) 0
  , Monkey [93, 69, 76, 66, 89, 54, 59, 94] (+7) (divisibleBy 7) (5,2) 0
  , Monkey [80, 54, 58, 75, 99] (*17) (divisibleBy 11) (1,6) 0
  , Monkey [69, 70, 85, 83] (+8) (divisibleBy 19) (2,7) 0
  , Monkey [89] (+6) (divisibleBy 2) (0,1) 0
  , Monkey [62, 80, 58, 57, 93, 56] (^2) (divisibleBy 13) (6,4) 0
  ]

magicNumber = 17 * 3 * 5 * 7 * 11 * 19 * 2 * 13

doRound :: MState ()
doRound = do
  monkeys <- get
  let turns = [doTurn n | n <- [0..length monkeys-1]]
  sequence_ turns

doRounds :: Int -> MState ()
doRounds = flip replicateM_ doRound

doMath :: Int
doMath = product $ take 2 $ reverse $ sort $ map countInspections monkeys
  where monkeys = execState (doRounds 10000) initialState

solve :: IO ()
solve = print doMath
