module WordSegmenterLib where

import Control.Monad

--Takes a number of characters to start at, a string, and a dictionary
--Returns the next word (if there is one) and rest of the string
findWord :: Int -> String -> [String] -> ([Maybe String], String)
findWord startChars str dict =
  if startChars > length str
    then ([Nothing], str)
  else
    if elem (take startChars str) dict
      then ([Just $ take startChars str], drop startChars str)
    else findWord (startChars + 1) str dict

findAllWords :: Int -> String -> [String] -> [([Maybe String], String)]
findAllWords startChars str dict =
  if startChars > length str
    then []
  else (findWord startChars str dict) : findAllWords (startChars + 1) str dict

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) =
  if elem x xs
    then removeDuplicates xs
  else x:removeDuplicates xs

findAllWordsWithoutDuplicates :: Int -> String -> [String] -> [([Maybe String], String)]
findAllWordsWithoutDuplicates startChars str dict =
  filter (not . elem Nothing . fst) $ removeDuplicates $ findAllWords startChars str dict

findPhrases :: ([Maybe String], String) -> [String] -> [([Maybe String], String)]
findPhrases (soFar, rest) dict =
  let nextWords = findAllWordsWithoutDuplicates 1 rest dict in
    map (\(x,y) -> (soFar ++ x, y)) nextWords

firstWords :: String -> [String] -> [([Maybe String], String)]
firstWords str dict = findAllWordsWithoutDuplicates 1 str dict

findMorePhrases :: [([Maybe String], String)] -> [String] -> [([Maybe String], String)]
findMorePhrases soFar dict =
  if any ((== "") . snd) soFar
    then soFar
    else (`findMorePhrases` dict) $ (join $ map (`findPhrases` dict) soFar)

findAllPhrases :: String -> [String] -> [([Maybe String], String)]
findAllPhrases str dict = findMorePhrases (firstWords str dict) dict

getFinalPhrase :: String -> [String] -> [Maybe String]
getFinalPhrase str dict = fst . head . filter ((== "") . snd) $ findAllPhrases str dict

getString :: String -> [String] -> String
getString str dict = unwords $ map (\(Just a) -> a) $ getFinalPhrase str dict
