-- Unparse expressions for debug display.
--
-- You should not need to modify this for the regular lab.
--
module Unparse (
  unparse
) where

import           Data.List
import           Lab6

unparse :: MathExp -> String
unparse (Let names assigns main) =
    "let " ++ commaList names ++ " = " ++
    (commaList . map unparseMathExp $ assigns) ++ " in " ++
    unparseMathExp main
    where
        commaList [str] =  str
        commaList strs  = "(" ++ intercalate ", " strs ++ ")"
unparse mathExp = unparseMathExp mathExp

unparseMathExp :: MathExp -> String
unparseMathExp (Number  n)        = show n
unparseMathExp (Var     name)     = name
unparseMathExp (Neg     e1)       = "-"   ++ unparse e1
unparseMathExp (Plus    e1 e2)    = "("   ++ unparse e1 ++ " + "     ++ unparse e2 ++ ")"
unparseMathExp (Minus   e1 e2)    = "("   ++ unparse e1 ++ " - "     ++ unparse e2 ++ ")"
unparseMathExp (Mult    e1 e2)    = "("   ++ unparse e1 ++ " * "     ++ unparse e2 ++ ")"
unparseMathExp (Div     e1 e2)    = "("   ++ unparse e1 ++ " / "     ++ unparse e2 ++ ")"
unparseMathExp (Pow     e1 e2)    = "("   ++ unparse e1 ++  "^"      ++ unparse e2 ++ ")"
unparseMathExp (Cond    e1 e2 e3) = "if " ++ unparse e1 ++  " then " ++ unparse e2 ++ " else " ++ unparse e3
unparseMathExp (Leq     e1 e2)    = "("   ++ unparse e1 ++  "<="     ++ unparse e2 ++ ")"
unparseMathExp (Geq     e1 e2)    = "("   ++ unparse e1 ++  ">="     ++ unparse e2 ++ ")"
unparseMathExp (Less    e1 e2)    = "("   ++ unparse e1 ++  "<"      ++ unparse e2 ++ ")"
unparseMathExp (Greater e1 e2)    = "("   ++ unparse e1 ++  ">"      ++ unparse e2 ++ ")"
unparseMathExp (Equal   e1 e2)    = "("   ++ unparse e1 ++  "=="     ++ unparse e2 ++ ")"
unparseMathExp (Neq     e1 e2)    = "("   ++ unparse e1 ++  "/="     ++ unparse e2 ++ ")"
unparseMathExp (Not     e1)       = "not" ++ unparse e1
