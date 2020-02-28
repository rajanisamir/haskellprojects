--undefined{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module AbstractInteger where

-- Here are some definations for AbstractNatural.
-- You will probably define your AbstractInteger based on
-- AbstractNatural.

data AbstractNatural = Zero | S AbstractNatural
    deriving (Show)

-- Once we tell Haskell that AbstractNatural can do equality
-- comparisons and how AbstractNatural is totally ordered, we
-- get other functions for free, like /= and >= and > and <
--
-- You may not need these so I've left them commented out, but
-- you should understand why they work.
--
instance Eq AbstractNatural where
  Zero == Zero = True
  Zero == S _  = False
  S _  == Zero = False
  S x  == S y  = x == y
--
instance Ord AbstractNatural where
  Zero <= Zero = True
  Zero <= S _  = True
  S _  <= Zero = False
  S x  <= S y  = x <= y

successorNat :: AbstractNatural -> AbstractNatural
successorNat = S

predecessorNat :: AbstractNatural -> AbstractNatural
predecessorNat Zero  = Zero
predecessorNat (S x) = x


-- Figure out how you will define integers...

data AbstractInteger = Pos AbstractNatural
                     | Neg AbstractNatural
    deriving (Show)

-- ...then fill out the functions below for your AbstractInteger type.

--Computes the next abstract integer.
successor :: AbstractInteger -> AbstractInteger
successor (Neg (S n)) = Neg n
successor (Neg Zero)  = Pos (S Zero)
successor (Pos n)     = Pos $ S n

--Computes the previous abstract integer.
predecessor :: AbstractInteger -> AbstractInteger
predecessor (Neg n)     = Neg (S n)
predecessor (Pos Zero)  = Neg (S Zero)
predecessor (Pos (S n)) = Pos n

-- Be sure to add type declarations to all these functions too.

--Computes the negative of an abstract integer.
negator :: AbstractInteger -> AbstractInteger
negator (Pos x)   = Neg x
negator (Neg x)   = Pos x

--Computes the absolute value of an abstract integer.
absolute :: AbstractInteger -> AbstractInteger
absolute (Neg x)  = Pos x
absolute (Pos x)  = Pos x

--Computes the sum of two abstract integers.
add :: AbstractInteger -> AbstractInteger -> AbstractInteger
add (Pos Zero) y  = y
add (Neg Zero) y  = y
add (Pos (S x)) y = successor (add (Pos x) y)
add (Neg (S x)) y = negator (add (Pos (S x)) (negator y))

--Computes the difference of two abstract integers.
difference :: AbstractInteger -> AbstractInteger -> AbstractInteger
difference x y = add x (negator y)

--Computes the product of two abstract integers.
multiply :: AbstractInteger -> AbstractInteger -> AbstractInteger

multiply (Pos Zero) (Pos y)  = Pos Zero
multiply (Pos (S x)) (Pos y) = add (Pos y) (multiply (Pos x) (Pos y))

multiply (Neg x) (Neg y) = multiply (Pos x) (Pos y)
multiply (Pos x) (Neg y) = negator (multiply (Pos x) (Pos y))
multiply (Neg x) (Pos y) = negator (multiply (Pos x) (Pos y))

-- To define division and modulo, you will probably need
-- comparison functions: == <= < > >=.
--
-- If you just provide == and <= below, Haskell will give
-- you the rest for free.

instance Eq AbstractInteger where
    Pos x == Pos y = (x == y)
    Neg x == Neg y = (x == y)
    Pos x == Neg y = and [x == Zero, y == Zero]
    Neg x == Pos y = and [x == Zero, y == Zero]

instance Ord AbstractInteger where
    Pos x <= Pos y = x <= y
    Neg x <= Neg y = x >= y
    Pos x <= Neg y = and [x == Zero, y == Zero]
    Neg x <= Pos y = True

--Performs Euclidean division on two abstract integers.
divide :: AbstractInteger -> AbstractInteger -> AbstractInteger
divide (Pos Zero) (Pos y) = Pos Zero
divide (Neg Zero) (Pos y) = Neg Zero

divide (Pos x) (Pos y) =
  if (Pos x) < (Pos y)
    then Pos Zero
  else
    add (Pos (S Zero)) (divide (difference (Pos x) (Pos y)) (Pos y))

divide (Neg x) (Pos y) =
    add (Neg (S Zero)) (divide (add (Neg x) (Pos y)) (Pos y))

divide (Neg x) (Neg y) = negator $ divide (Neg x) (Pos y)
divide (Pos x) (Neg y) = negator $ divide (Pos x) (Pos y)

--Computes the remainder when an abstract integer is divided by another abstract integer.
modulo :: AbstractInteger -> AbstractInteger -> AbstractInteger
modulo x y = difference x (multiply (divide x y) y)

--Converts an integer to an abstract natural.
toAbstractNatural :: Integer -> AbstractNatural
toAbstractNatural 0 = Zero
toAbstractNatural x = S $ toAbstractNatural (x - 1)

--Converts an integer to an abstract integer.
toAbstract :: Integer -> AbstractInteger
toAbstract x =
  if x >= 0
    then Pos (toAbstractNatural x)
  else
    Neg (toAbstractNatural (-x))

--Converts an abstract natural to an integer.
fromAbstractNatural :: AbstractNatural -> Integer
fromAbstractNatural Zero = 0
fromAbstractNatural (S x) = 1 + (fromAbstractNatural x)

--Converts an abstract integer to an integer.
fromAbstract :: AbstractInteger -> Integer
fromAbstract (Pos x) = fromAbstractNatural x
fromAbstract (Neg x) = -(fromAbstractNatural x)

-- Take a list of strings, calculate, and return a string result.
-- You should not need to modify this, but you may eta-reduce it if you like.
evaluateRPN :: [String] -> AbstractInteger
evaluateRPN inputList = evalRPNStack [] inputList

-- The core of the RPN caluculator, Stack -> InputList -> Output
-- You will need to provide more cases.
evalRPNStack :: [AbstractInteger] -> [String] -> AbstractInteger
evalRPNStack stack inputList =
    case (stack, inputList) of
        ( x:_,           [] )            -> x -- No more input, return top of stack.
        ( y:x:stackRest, "+":inputRest ) -> evalRPNStack (add x y        : stackRest) inputRest
        ( y:x:stackRest, "*":inputRest ) -> evalRPNStack (multiply x y   : stackRest) inputRest
        ( y:x:stackRest, "-":inputRest ) -> evalRPNStack (difference x y : stackRest) inputRest
        ( y:x:stackRest, "/":inputRest ) -> evalRPNStack (divide x y     : stackRest) inputRest
        ( x:stackRest, "abs":inputRest ) -> evalRPNStack (absolute x   : stackRest) inputRest
        ( y:x:stackRest, "%":inputRest ) -> evalRPNStack (modulo x y     : stackRest) inputRest
        -- ...add more cases here...
        -- This last case handles numeric inputs, "0" "-2" "34" etc...
        ( _,          numStr:inputRest ) -> evalRPNStack (toAbstract (read numStr) : stack) inputRest

-- Convenience constructors. Handy for testing in ghci.
-- Define zero after you've written your definition of AbstractInteger.
-- Once you define zero you should get the rest for free.
zero  = Pos Zero
one   = successor zero
two   = successor one
three = successor two
four  = successor three
five  = successor four
six   = successor five
seven = successor six
eight = successor seven
nine  = successor eight
ten   = successor nine

negativeOne   = predecessor zero
negativeTwo   = predecessor negativeOne
negativeThree = predecessor negativeTwo
negativeFour  = predecessor negativeThree
negativeFive  = predecessor negativeFour
negativeSix   = predecessor negativeFive
negativeSeven = predecessor negativeSix
negativeEight = predecessor negativeSeven
negativeNine  = predecessor negativeEight
negativeTen   = predecessor negativeNine
