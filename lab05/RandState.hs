module RandState where

import System.Random

newtype RandState a = RandState {
  runRandState :: StdGen -> (a, StdGen)
}

instance Functor RandState where
  fmap f ra = RandState $ \gen ->
    let (rand,gen') = runRandState ra gen
    in (f rand,gen')

instance Applicative RandState where
  pure val = RandState $ \gen -> (val, gen)
  (<*>) rf ra = RandState $ \gen ->
    let (f, gen')  = runRandState rf gen
        (a, gen'') = runRandState ra gen'
    in  (f a, gen'')

instance Monad RandState where
  (>>=) ra f = RandState $ \gen ->
    let (a, gen')  = runRandState ra gen
        rb         = f a
        (b, gen'') = runRandState rb gen'
    in  (b, gen'')

{- primitive manipulation functions -}

get :: RandState StdGen
get = RandState $ \gen -> (gen, gen)

put :: StdGen -> RandState ()
put gen' = RandState $ \gen -> ((), gen')

-- runRandom runs a RandState monad, given an initial random number generator
-- runRandom is equivalent to evalState
runRandom :: RandState a -> StdGen -> a
runRandom (RandState f) s = fst $ f s

-- rand is a helper function that generates a random instance of any
--  type in the Random class, using the RandState monad.
rand :: Random a => RandState a
rand = do
    gen <- get
    let (x, gen') = random gen
    put gen'
    return x
