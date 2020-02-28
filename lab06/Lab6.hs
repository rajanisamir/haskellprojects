-- Write your parser in this file.

module Lab6 (
  Name,
  Number,
  MathExp(..),
  parse
) where

import           Control.Applicative          hiding (many)
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Ord
import           Text.ParserCombinators.ReadP

type Name   = String  -- Variable names are strings.
type Number = Int     -- The kind of number in our language.

-- A math expression is a number, a variable,
-- a negation of a math expression, or any of
-- the four major operations plus power on a
-- pair of math expressions.
--
-- In the actual parser:
--   1. Precedence should be standard order of operations.
--   2. Negation should only precede a number, a variable, or
--      a parenthetical expression.
--   3. A variable starts with a lowercase letter and after
--      the first letter is any alphanumeric character a-z A-Z 0-9.
--
-- Your parser does _not_ need an explicit tokenization step like Lab 3.
-- In the functional parsing paradigm, tokenization+parsing occur
-- simultaneously.

data MathExp
    = Number  Number
    | Bool    Bool
    | Var     Name
    | Neg     MathExp
    | Plus    MathExp MathExp
    | Minus   MathExp MathExp
    | Mult    MathExp MathExp
    | Div     MathExp MathExp
    | Pow     MathExp MathExp
    | Let     [Name] [MathExp] MathExp
    | Cond    MathExp MathExp MathExp
    | Leq     MathExp MathExp
    | Geq     MathExp MathExp
    | Less    MathExp MathExp
    | Greater MathExp MathExp
    | Equal   MathExp MathExp
    | Neq     MathExp MathExp
    | Not     MathExp
    deriving (Eq, Show)

--parses a variable
parseVar :: ReadP Name
parseVar = do
  first <- satisfy (isLower)
  rest  <- getRest
  pure (first:rest)
    where
      getRest = do
        restStr <- look
        case take 2 restStr of
          "in" -> pure []
          _ -> liftA2 (:) (satisfy (\x -> isAlpha x || isDigit x)) getRest <++ pure []

--parses a plus or minus operation
parsePlusMinus :: ReadP (MathExp -> MathExp -> MathExp)
parsePlusMinus =
  (\[_,op,_] -> op) <$>
    (sequence $ skipBetweenBinary
    [(Plus <$ char '+') +++ (Minus <$ char '-')])

--parses a multiplication or division operation
parseMultDiv :: ReadP (MathExp -> MathExp -> MathExp)
parseMultDiv =
  (\[_,op,_] -> op) <$>
  (sequence $ skipBetweenBinary
  [(Mult <$ char '*') +++ (Div <$ char '/')])

--parses a power operation
parsePow :: ReadP (MathExp -> MathExp -> MathExp)
parsePow =
  (\[_,op,_] -> op) <$>
  (sequence $ skipBetweenBinary
  [Pow <$ char '^'])

--parses a number
parseNum :: ReadP MathExp
parseNum = do
  skipSpaces
  num <- Number <$> (read <$> munch1 isDigit)
  pure num

--parses a negative sign
parseNeg :: ReadP (MathExp -> MathExp)
parseNeg = do
  skipSpaces
  neg <- (Neg <$ satisfy (== '-'))
  pure neg

--parses a math expression wrapped in parentheses
parsePar :: ReadP MathExp
parsePar = do
  sequence $ skipBetween
    [() <$ char '(']
  expression <- parseMathExp
  sequence $ skipBetween
    [() <$ char ')']
  return expression

--parses a comparison operator
parseComparison :: ReadP (MathExp -> MathExp -> MathExp)
parseComparison = (Geq     <$ string ">=") <++
                  (Leq     <$ string "<=") <++
                  (Equal   <$ string "==") <++
                  (Neq     <$ string "/=") <++
                  (Greater <$ char   '>') <++
                  (Less    <$ char   '<')

--parses a not operator
parseNot :: ReadP (MathExp -> MathExp)
parseNot = (Not <$ string "not")

--parses an if...then...else expression
parseCond :: ReadP MathExp
parseCond = do
  sequence $ skipBetween
    [() <$ string "if"]
  predicate <- parsePred
  sequence $ skipBetween
    [() <$ string "then"]
  expression <- parseMathExp
  sequence $ skipBetween
    [() <$ string "else"]
  alternative <- parseMathExp
  pure $ Cond predicate expression alternative

--parses the predicate of an if clause
parsePred :: ReadP MathExp
parsePred = do
  lhs <- parseMathExp
  skipSpaces
  op <- parseComparison
  skipSpaces
  rhs <- parseMathExp
  pure $ op lhs rhs

--parses an expression with only parentheses, negative signs,
--exponents, numbers, and variables
parseHighPrec :: ReadP MathExp
parseHighPrec = (do
                  parseNeg
                  (Neg <$> parseHighPrec)) <++
                (do
                  leftSide  <- parseCond <++ parseLet <++ parseNum
                               +++ parsePar +++ (Var <$> parseVar)
                  power     <- parsePow
                  rightSide <- parseHighPrec
                  pure $ power leftSide rightSide) <++
                (do
                  expression <- parseCond <++ parseLet <++ parseNum
                                +++ parsePar +++ (Var <$> parseVar)
                  pure expression)

--parses a math expression
parseMathExp :: ReadP MathExp
parseMathExp = parseLet <++ chainl1 (chainl1 parseHighPrec parseMultDiv) parsePlusMinus

--helper function that intersperses a value into a list and puts that
--value at the beginning and the end of that list
interspersePlus :: a -> [a] -> [a]
interspersePlus a as = [a] ++ (intersperse a as) ++ [a]

--helper function that adds skipSpaces between unit parsers
skipBetween :: [ReadP ()] -> [ReadP ()]
skipBetween parsers = interspersePlus skipSpaces parsers

--helper function that adds skipSpaces between binary parsers
skipBetweenBinary :: [ReadP (MathExp -> MathExp -> MathExp)] ->
                     [ReadP (MathExp -> MathExp -> MathExp)]
skipBetweenBinary parsers = interspersePlus (Plus <$ skipSpaces) parsers

--parses a let expression of one variable
parseLetSingle :: ReadP MathExp
parseLetSingle = do
  sequence $ skipBetween
    [() <$ string "let"]
  var <- parseVar
  sequence $ skipBetween
    [() <$ char '=']
  def <- parseMathExp
  sequence $ skipBetween
    [() <$ string "in"]
  expression <- parseMathExp
  pure $ Let [var] [def] expression

--parses a let expression of multiple variables
parseLetMultiple :: ReadP MathExp
parseLetMultiple = do
  sequence $ skipBetween
    [() <$ string "let",
     () <$ char '(']
  vars <- sepBy parseVar (sequence $ skipBetween [() <$ char ','])
  sequence $ skipBetween
    [() <$ char ')',
     () <$ char '=',
     () <$ char '(']
  defs <- sepBy parseMathExp (char ',')
  sequence $ skipBetween
    [() <$ char ')',
     () <$ string "in"]
  expression <- parseMathExp
  pure $ Let vars defs expression

--parses a let expression
parseLet :: ReadP MathExp
parseLet = parseLetSingle +++ parseLetMultiple

--parses a top-level expression
parseTLE :: ReadP MathExp
parseTLE = do
  tle <- parseMathExp
  skipSpaces
  return tle

-- Run the parser on a given string.
--
-- You should not modify this function. Grading may
-- look for the specific messages below.

parse :: String -> Either String MathExp
parse str =
    case (completeParses, incompleteParses) of
        ([(result, "")], _  ) -> Right result  -- Only complete result.
        ([]            , [] ) -> Left $ "No parse."
        ([]            , _:_) -> Left $ "Incomplete parse. Unparsed: " ++ show leastRemaining
        (_:_           , _  ) -> Left $ "Ambiguous parse: " ++ show completeParses
    where
        parses = readP_to_S parseTLE str
        (completeParses, incompleteParses) =
            partition (\(_, remaining) -> remaining == "") parses
        leastRemaining = minimumBy (comparing length) . map snd $ incompleteParses
