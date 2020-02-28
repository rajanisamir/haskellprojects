import Data.Char
import Data.List
import System.IO
import Data.Map.Strict hiding (filter, map)
import Control.Arrow

--gets rid of characters in a string that aren't letters or spaces
lettersAndSpaces :: String -> String
lettersAndSpaces = filter (\x -> isLetter x || (==) ' ' x)

--converts string to lowercase
strToLower :: String -> String
strToLower = map toLower

--helper function for splitWords that gets the next word from a string
getWord :: String -> (String, String)
getWord (c:cs) =
  if isLetter c
  then (c:(fst $ getWord cs), snd $ getWord cs)
  else ([], cs)
getWord [] = ([], [])

--converts a string into a list of words
splitWords :: String -> [String]
splitWords [] = []
splitWords (c:cs) =
  if isLetter c
    then (fst $ getWord (c:cs)) : (splitWords $ snd $ getWord (c:cs))
  else splitWords cs

--removes all elements in a list which are replicated
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) =
  if elem x xs
    then removeDuplicates xs
  else x:removeDuplicates xs

listToPairs :: [String] -> [(String, String)]
listToPairs =
  toAscList . unionsWith ((++) . (++ " ")) . map (\x -> singleton (sort x) (x))

pairsToPermutations :: [(String, String)] -> [[String]]
pairsToPermutations =
  filter ((> 1) . length) . map removeDuplicates . (map (splitWords . snd))

formatOutput :: [[String]] -> String
formatOutput = intercalate "\n" . sort . map (intercalate ", " . sort)

getPermutations :: String -> String
getPermutations =
  lettersAndSpaces    >>>
  strToLower          >>>
  splitWords          >>>
  listToPairs         >>>
  pairsToPermutations >>>
  formatOutput

main :: IO ()
main = do
  input <- getContents
  putStr $ getPermutations input
  hFlush stdout
