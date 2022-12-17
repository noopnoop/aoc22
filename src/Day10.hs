module Day10 (solve) where
import Text.Parsec.ByteString.Lazy (Parser, parseFromFile)
import Text.Parsec (optionMaybe, sepBy, (<|>))
import Text.Parsec.Combinator (many1, sepEndBy)
import Text.Parsec.Char (digit)
import Text.Parsec (char, string, space)
import Text.Parsec (lower)
import Debug.Trace (trace, traceShowId, traceShowM)
import Control.Monad.State.Lazy (State, modify, get, MonadState (..), gets, evalState)
import Data.List (partition)

type CPU x = State CPUState x
type Stack = [(Int,Instruction)]
type Instruction = Int -> Int
data CPUState = CPUState 
  { stack    :: Stack
  , register :: Int
}

beginExecution :: Int -> Instruction -> CPU ()
beginExecution delay ins = do
  (CPUState oldStack reg) <- get
  let newStack = (delay, ins) : oldStack
  put $ CPUState newStack reg

getReady :: Stack -> (Stack,Stack)
getReady = partition $ \(n,_) -> n == 0

tick :: Stack -> Stack
tick = map $ \(n,ins) -> (n-1,ins)

tickM :: CPU ()
tickM = do
  (CPUState oldStack oldReg) <- get
  put $  CPUState (tick oldStack) oldReg

finishExecution :: CPU ()
finishExecution = do
  tickM
  (CPUState oldStack oldReg) <- get
  let (toExecute, newStack) = getReady oldStack
  if null toExecute
    then return ()
    else put $ CPUState newStack $ applyAll oldReg $ map snd toExecute

toCycle :: Int -> Instruction -> CPU Int
toCycle n ins = finishExecution >> beginExecution n ins >> gets register

parseInt :: Parser Int
parseNoop :: Parser (CPU Int)
parseAddx :: Parser (CPU Int)
parseInstruction :: Parser (CPU Int)
parseDataset :: Parser [CPU Int]
parseInt = read <$> do
  sign <- optionMaybe $ char '-'
  nums <- many1 digit
  case sign of
    Nothing   -> return nums
    Just dash -> return $ dash : nums
parseNoop = string "noop" >> return (toCycle 1 id)
parseAddx = do
  _ <- string "addx"
  _ <- space
  n <- parseInt
  return $ toCycle 2 (+n)
parseInstruction = parseNoop <|> parseAddx
parseDataset = parseInstruction `sepEndBy` char '\n'

applyAll :: a -> [a -> a] -> a
applyAll x []       = x
applyAll x (fn:fns) = applyAll (fn x) fns

magicNumbers :: [Int]
magicNumbers = [20,60,100,140,180,220]

initialState :: CPUState
initialState = CPUState [] 1

doMath :: [CPU Int] -> Int
doMath instructions = sum $ traceShowId $ map signalStrength magicNumbers
  where signalStrength cycleNum = cycleNum * (history !! (cycleNum - 1))
        history = flip evalState initialState $ sequence instructions

solve :: IO ()
solve = do
  dataset <- parseFromFile parseDataset "input.txt"
  case dataset of
    Left err   -> print err
    Right good -> traceShowM (length good) >> print (doMath good)