import RandState
import System.Environment
import System.IO
import System.Random
import Control.Monad

-- Data types to represent playing cards
data CardValue
    = King
    | Queen
    | Jack
    | NumberCard Int  -- From 1 to 10
    deriving (Show, Eq)

data CardSuit
    = Hearts
    | Diamonds
    | Spades
    | Clubs
    deriving (Show, Eq)

data PlayingCard =
    PlayingCard CardValue CardSuit
    deriving (Eq)

type Deck = [PlayingCard]


instance Show PlayingCard where
    show (PlayingCard value suit) =
        valueStr value ++ suitStr suit
        where
            suitStr Hearts   = "\x1B[31m♥\x1B[0m" -- red in terminal
            suitStr Diamonds = "\x1B[31m♦\x1B[0m" -- red in terminal
            suitStr Spades   = "♠"
            suitStr Clubs    = "♣"
            -- suitStr Hearts   = "H"  -- uncomment if you don't have Unicode
            -- suitStr Diamonds = "D"
            -- suitStr Spades   = "S"
            -- suitStr Clubs    = "C"
            valueStr King           = "K"
            valueStr Queen          = "Q"
            valueStr Jack           = "J"
            valueStr (NumberCard n) = show n


-- fullCardDeck is a deck of cards, 52 in total, with a King, a Queen,
-- a Jack and NumberCards from 1 to 10 for each suit.
fullCardDeck :: Deck
fullCardDeck =
    [ PlayingCard v s | v <- allVals, s <- allSuits ]
    where
        allVals  = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allSuits = [Hearts, Diamonds, Spades, Clubs]

--helper functions

--removes the element of a list at a given index
removeAtIndex :: Int -> [a] -> [a]
removeAtIndex index list = (take index list) ++ (drop (index + 1) list)

-- basic random functions

randR :: Random a => (a, a) -> RandState a
randR (low, high) = do
  gen <- get
  let (x, gen') = randomR (low, high) gen
  put gen'
  return x

rollTwoDice :: RandState Int
rollTwoDice = do
  roll1 <- randR (1, 6)
  roll2 <- randR (1, 6)
  return $ roll1 + roll2

removeCard :: [PlayingCard] -> RandState (PlayingCard, [PlayingCard])
removeCard cards = do
  randIndex <- randR (0, length cards - 1)
  return (cards !! randIndex, removeAtIndex randIndex cards)

shuffleDeck :: [PlayingCard] -> RandState [PlayingCard]
shuffleDeck cards =
  case cards of
    [] ->
      do
        pure []
    deck ->
      do
        (card,rest) <- removeCard deck
        (:) <$> pure card <*> shuffleDeck rest

shuffleADeck :: RandState [PlayingCard]
shuffleADeck = shuffleDeck fullCardDeck

shuffleNTimes :: Int -> StdGen -> IO ()
shuffleNTimes nTimes gen = do
  let decks = runRandom (replicateM nTimes shuffleADeck) gen
  mapM_ (putStrLn . show) decks

rollTwoDiceNTimes :: Int -> StdGen -> IO ()
rollTwoDiceNTimes nTimes gen = do
  let rolls = runRandom (replicateM nTimes rollTwoDice) gen
  mapM_ (putStrLn . show) rolls

usage :: String
usage =
    "Lab 5: Randomizer\n" ++
    "\n" ++
    "$ ./Lab5 shuffle 600      # 600 times: output a full deck shuffle\n" ++
    "$ ./Lab5 rollTwoDice 800  # 800 times: output the sum of rolling two dice\n" ++
    "\n"

main :: IO ()
main = do
    gen  <- newStdGen
    args <- getArgs
    case args of
        ["shuffle",     nTimes] -> shuffleNTimes     (read nTimes) gen
        ["rollTwoDice", nTimes] -> rollTwoDiceNTimes (read nTimes) gen
        _                       -> putStrLn usage
