module Day7 (solve) where
import Data.List (find)

type SystemObject = Either Directory File

data File = File
  { fileName :: String
  , size :: Int
  } deriving (Eq,Show)

data Directory = Directory
  { parent   :: Directory
  , dirName     :: String
  , children :: [SystemObject]
  } 

isNamed :: String -> Directory -> Bool
isNamed str (Directory _ nam _) = str == nam

tryLeft :: (a -> Bool) -> Either a b -> Bool
tryLeft fn (Left a) = fn a
tryLeft _ _         = False

tryRight :: (b -> Bool) -> Either a b -> Bool
tryRight fn (Right b) = fn b
tryRight _ _         = False

cdIn :: String -> Directory -> Directory
cdIn str (Directory _ nam chld) =
  case find (tryLeft $ isNamed str) chld of
    Nothing -> error $ "cdIn error: dir " ++ nam ++ " not found"
    Just dir -> undefined --faeiogheioghaoigheioa


solve :: IO ()
solve = undefined