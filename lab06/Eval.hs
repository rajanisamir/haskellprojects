module Eval (
  EvalResult,
  eval
) where

import           Control.Applicative
import           Lab6

-- Error message or result number.
type EvalResult = Either String MathExp

type Bindings = [(Name, MathExp)]

-- Return wrapped list of bare results if all inputs are Right.
-- Otherwise, returns the first Left error message.
allRight :: [EvalResult] -> Either String [MathExp]
allRight = foldr (liftA2 (:)) (Right [])

-- Returns either an error string or a resulting integer.
eval :: Bindings -> MathExp -> EvalResult
eval bindings (Let names mathExps mainExp)
    | length names == length mathExps = performEval
    | otherwise                       = Left errorMsg
    where
        performEval = do
            bindings <- bindingsOrError
            eval bindings mainExp
        bindingsOrError =
            zip names <$> (allRight . map (eval bindings) $ mathExps)
        errorMsg =
            "must assign " ++ show (length names) ++
            " names but given " ++ show (length mathExps) ++
            " expressions"
eval bindings mathExp =
  let
      unOp op e          = op <$>    (recurseNum bindings e)
      binOp op e1 e2     = liftA2 op (recurseNum bindings e1) (recurseNum bindings e2)
      unOpBool op e      = op <$>    (recurseBool bindings e)
  in
  case mathExp of
      Number n -> Right (Number n)
      Bool b   -> Right (Bool b)
      Var name ->
          case lookup name bindings of
              Just n  -> Right n
              Nothing -> Left $ "could not find variable \"" ++ name ++ "\""
      Neg   e     -> Number <$> unOp negate e
      Plus  e1 e2 -> Number <$> binOp (+)      e1 e2
      Minus e1 e2 -> Number <$> binOp subtract e2 e1
      Mult  e1 e2 -> Number <$> binOp (*)      e1 e2
      Div   e1 e2 ->
          if recurseNum bindings e2 == Right 0
          then Left "division by zero"
          else Number <$> binOp quot e1 e2
      Pow   e1 e2 ->
          case recurseNum bindings e2 of
              Right pow -> if pow < 0
                           then Left "negative exponent"
                           else Number <$> binOp (^) e1 e2
              Left err  -> Left err
      Cond e1 e2 e3 ->
          case recurseBool bindings e1 of
            Left err    -> Left err
            Right True  -> eval bindings e2
            Right False -> eval bindings e3
      Leq e1 e2     -> Bool <$> binOp (<=) e1 e2
      Geq e1 e2     -> Bool <$> binOp (>=) e1 e2
      Less e1 e2    -> Bool <$> binOp (<)  e1 e2
      Greater e1 e2 -> Bool <$> binOp (>)  e1 e2
      Neq e1 e2     -> Bool <$> binOp (/=) e1 e2
      Equal e1 e2   -> Bool <$> binOp (==) e1 e2
      Not e         -> Bool <$> unOpBool not e

recurseNum :: Bindings -> MathExp -> Either String Number
recurseNum bindings mathExp =
  case eval bindings mathExp of
    Right (Number n) -> Right n
    Left err         -> Left err

recurseBool :: Bindings -> MathExp -> Either String Bool
recurseBool bindings mathExp =
  case eval bindings mathExp of
    Right (Bool b)    -> Right b
    Left err          -> Left err
