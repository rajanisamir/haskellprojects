import System.IO
import WordSegmenterLib

main = do
    handle   <- openFile "/usr/share/dict/words" ReadMode
    dict <- lines <$> hGetContents handle
    str <- getLine
    putStr $ getString str dict
    hClose handle
