import System.IO

import Lab3 --make sure this is fine

main :: IO ()
main = do
     putStr "> "
     hFlush stdout
     line <- getLine
     putStrLn $ (show $ fst $ compute line) ++ "/" ++ (show $ snd $ compute line)
     main
