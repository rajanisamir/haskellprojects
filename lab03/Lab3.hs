module Lab3 where

import Data.List
import Debug.Trace
import Data.Char

data ArithExp = Number Int Int
              | Plus ArithExp ArithExp
              | Mult ArithExp ArithExp
              | Div ArithExp ArithExp
    deriving (Eq, Show)

--evaluates an arithmetic expression to produce a fraction represented by a pair of integers
eval :: ArithExp -> (Int, Int)
eval (Number a b) = (a, b)
eval (Plus a b) = ((fst $ eval a) * (snd $ eval b) + (fst $ eval b) * (snd $ eval a),
                  (snd $ eval a) * (snd $ eval b))
eval (Mult a b) = ((fst $ eval a) * (fst $ eval b),
                   (snd $ eval a) * (snd $ eval b))
eval (Div a b)  = ((fst $ eval a) * (snd $ eval b),
                   (snd $ eval a) * (fst $ eval b))

--reduces a fraction represented by a pair of integers
reduce :: (Int, Int) -> (Int, Int)
reduce (a, b) = (div a den, div b den)
  where den = if (a > 0) && (b > 0) then gcd a b
              else (-1) * gcd a b

data Token
  = NumTok String
  | AddTok
  | MulTok
  | DivTok
  | ParTok [Token]
  deriving (Show, Eq)

--tokenizes a parenthetical expression
--returns a token list representing the contents of the parentheses and the remainder of the string
tokenizePar :: String -> ([Token], String)
tokenizePar [] = ([], "")
tokenizePar (')':rest) = ([], rest)
tokenizePar (chars) =
  (unit ++ (fst $ tokenizePar rest), snd $ tokenizePar rest)
  where (unit, rest) = tokenizeSingle (chars)

--given a string, tokenizes a number
--returns a string representing the number and the remainder of the string
tokenizeNum :: String -> (String, String)
tokenizeNum (num:rest) =
  if (isDigit num)
  then (num : (fst $ tokenizeNum rest), snd $ tokenizeNum rest)
  else ([], num:rest)
tokenizeNum [] = ([], [])

--given a string, tokenizes a single character
--returns a list of tokens and the remainder of the string
tokenizeSingle :: String -> ([Token], String)
tokenizeSingle (' ':rest) = tokenizeSingle rest
tokenizeSingle ('+':rest) = ([AddTok], rest)
tokenizeSingle ('*':rest) = ([MulTok], rest)
tokenizeSingle ('/':rest) = ([DivTok], rest)
tokenizeSingle (')':rest) = ([], rest)
tokenizeSingle ('(':rest) =
  ([ParTok (fst $ tokenizePar rest)], (snd $ tokenizePar rest))
tokenizeSingle ('-':rest) =
  ([NumTok ('-' : (fst $ tokenizeNum rest))], (snd $ tokenizeNum rest))
tokenizeSingle (num:rest) =
  ([NumTok (fst $ tokenizeNum (num:rest))], (snd $ tokenizeNum (num:rest)))

--turns a string into tokens representing each component of an expression
tokenize :: String -> [Token]
tokenize [] = []
tokenize (next:rest) =
  (fst $ tokenizeSingle (next:rest)) ++
  tokenize (snd $ tokenizeSingle (next:rest))

--recursively splits list in order of least precedence
--returns an arithmetic expression given a list of tokens
parse :: [Token] -> ArithExp
parse ((NumTok x) : []) = Number (read x) (1)
parse [ParTok x] = parse x
parse tokens =
  case findIndex (== AddTok) tokens of
    Just x  -> Plus (parse $ take x tokens)
                    (parse $ drop (x+1) tokens)
    Nothing ->
      case findIndex (== MulTok) tokens of
        Just x  -> Mult (parse $ take x tokens)
                        (parse $ drop (x+1) tokens)
        Nothing ->
          case findIndex (== DivTok) tokens of
            Just x  -> Div (parse $ take x tokens)
                           (parse $ drop (x+1) tokens)

--combines each step of the process to evaluate a string
compute :: String -> (Int, Int)
compute x = reduce $ eval $ parse $ tokenize x
