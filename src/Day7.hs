module Day7 (solve) where
import qualified Data.Map as M
import Data.List (isPrefixOf, intercalate, intersperse, sort)
import Text.Parsec.Prim (Parsec, getState, putState, runParser)
import Text.Parsec (char, string, lower, space, many1, noneOf, (<|>), digit, sepEndBy)
import GHC.Utils.Misc (split)
import Control.Monad (void)
import Text.Parsec.String (parseFromFile)
import Debug.Trace (traceShowId, traceShowM)
import Data.Maybe (fromMaybe)

type FS = M.Map String Int

getSize :: String -> FS -> Int
getSize dir fs = sum $ M.elems $ M.filterWithKey isChild fs
  where isChild name _ = dir `isPrefixOf` name

addDirSizes :: FS -> FS
addDirSizes fs = M.mapWithKey addSize fs
  where addSize str curr = 
          if isDir str
            then getSize str fs
            else curr

isDir :: String -> Bool
isDir str = last str == '/'

dirs :: FS -> FS
dirs = M.filterWithKey isDir'
  where isDir' str _ = isDir str

doMath :: FS -> Int
doMath fs = minimum $ M.elems $ M.filter (>= 30000000 - unused) directories
  where directories = dirs $ addDirSizes fs
        unused = 70000000 - fromMaybe 0 (M.lookup "/" directories)


data State = State FS String

initialState :: State
initialState = State (M.singleton "/" 0) "/"

type Parser = Parsec String State

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseLine :: Parser ()
parseLine = addItem
        <|> parseCommand

parseCommand :: Parser ()
parseCommand = do
  _ <- string "$ "
  void (string "ls") <|> parseCD

parseDataset :: Parser FS
parseDataset = do
  _ <- parseLine `sepEndBy` char '\n'
  State fs _ <- getState
  return fs

parseCD :: Parser ()
parseCD = do
  _   <- string "cd "
  dir <- many1 $ noneOf [ '\n' ]
  case dir of
    "/"  -> goHome
    ".." -> moveOut
    str  -> moveIn str

goHome :: Parser ()
goHome = do
  State fs _ <- getState
  putState $ State fs "/"

moveOut :: Parser ()
moveOut = do
  State fs oldCursor <- getState
  let newCursor = moveOutStr oldCursor
  putState $ State fs newCursor

-- >>> moveOutStr "/hello/world/"
-- "/hello/"
moveOutStr :: String -> String
moveOutStr = (++"/") . intercalate "/" . init . init . split '/'

moveIn :: String -> Parser ()
moveIn str = do
  State fs oldCursor <- getState
  let newCursor = oldCursor <> str <> "/"
  putState $ State fs newCursor

addItem :: Parser ()
addItem = addDir <|> addFile

addDir :: Parser ()
addDir = do
  _    <- string "dir "
  name <- many1 lower
  State oldFS cursor <- getState
  let newFS = M.insert (cursor <> name <> "/") 0 oldFS
  putState $ State newFS cursor

addFile :: Parser ()
addFile = do
  size <- parseInt
  _    <- space
  name <- many1 $ lower <|> char '.'
  State oldFS cursor <- getState
  let newFS = M.insert (cursor <> name) size oldFS
  putState $ State newFS cursor

solve :: IO ()
solve = do
  input <- readFile "input.txt"
  let dataset = runParser parseDataset initialState "input.txt" input
  case dataset of
    Left err -> print err
    Right good -> print $ doMath good