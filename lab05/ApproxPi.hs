import RandState
import System.Environment
import System.IO
import System.Random
import Control.Monad

-- Succeed if randomly chosen point from square is inside circumscribed circle
piTrial :: RandState Bool
piTrial = do
  x <- rand :: RandState Double
  y <- rand :: RandState Double
  pure $ x^2 + y^2 <= 1

-- Perform n trials of the RandState function provided as the second argument,
--  and give back the number of successful trials
-- Hint: To perform the n trials, you can either use sequence from
--       Control.Monad, or you can use recursion
bernoulliTrials :: Int -> RandState Bool -> RandState Int
bernoulliTrials nTimes rb = do
  results <- sequence $ replicate nTimes rb
  pure . length . filter (== True) $ results

-- Approximate pi using n randomly chosen points
-- Hint: You will probably need to use the fromIntegral function to
--       convert Int into Double.
approxPi :: Int -> RandState Double
approxPi nTimes = do
  nSuccessful <- bernoulliTrials nTimes piTrial
  pure $ 4 * (fromIntegral nSuccessful) / (fromIntegral nTimes)

usage :: String
usage =
    "Lab 5: Randomizer\n" ++
    "\n" ++
    "$ ./Lab5 approxPi 600 # 600 times: approximate pi using 600 Bernoulli Trials \n"

main :: IO ()
main = do
    gen  <- newStdGen
    args <- getArgs
    case args of
        [nTimes] -> putStrLn . show $ runRandom (approxPi $ read nTimes) gen
        _        -> putStrLn usage
