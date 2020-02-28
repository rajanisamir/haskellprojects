import Data.Char
import Data.List
import System.IO
import Control.Arrow
import Data.Map.Strict hiding (filter, map)

strToLower :: String -> String
strToLower = map toLower

--takes a string and returns the next word and the rest of the string
getWord :: String -> (String, String)
getWord (c:cs) =
  if isLetter c || (==) '-' c || (==) '\'' c
  then (c : (fst $ getWord cs), snd $ getWord cs)
  else ([], cs)
getWord [] = ([], [])

--takes a string and splits it into individual words
splitWords :: String -> [String]
splitWords [] = []
splitWords (c:cs) =
  if isLetter c || (==) '-' c || (==) '\'' c
    then (fst $ getWord (c:cs)) : (splitWords $ snd $ getWord (c:cs))
  else splitWords cs

--takes a list of words and gives a list of maps with words as keys and 1s as values
toMap :: [String] -> [Map String Int]
toMap (x:xs) = (singleton x 1) : toMap xs
toMap []     = []

--combines list of maps to get list of tuples with words and appearances
compressMap :: [Map String Int] -> [(String, Int)]
compressMap maps = toAscList $ unionsWith (+) maps

compressTuples :: [(String, Int)] -> [String]
compressTuples [] = []
compressTuples ((word, appearances):xs)
  = (word ++ " " ++ show appearances) : compressTuples xs

--converts to lowercase, sorts, and places line breaks between words in list to produce string
formatOutput :: [String] -> String
formatOutput = intercalate "\n" . sort

wordCount :: String -> String
wordCount =
  strToLower    >>>
  splitWords    >>>
  toMap         >>>
  compressMap   >>>
  compressTuples >>>
  formatOutput

main :: IO ()
main = do
  input <- getContents
  putStr $ wordCount input
  hFlush stdout
