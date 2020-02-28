module Tests where

import Lab3

data TestResult
    = Success
    | Failure String

isSuccess :: TestResult -> Bool
isSuccess Success = True
isSuccess _       = False

message :: TestResult -> String
message Success           = "Success!"
message (Failure message) = message

-- Test a function that takes one argument.
-- Usage: expect1 "myFunc" myFunc arg expectedOutput
expect1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> a -> b -> TestResult
expect1 funcName func input expectedOutput =
    if expectedOutput == actual then
        Success
    else
        Failure $
            "Expected " ++ evaledStr ++
            " to be " ++ show expectedOutput ++
            ", but got " ++ show actual
    where
        actual    = func input
        evaledStr = funcName ++ " " ++ show input


-- This is where you add new test cases.
tests :: [TestResult]
tests =
    [ expect1 "eval" eval
        (Number ((-3)) 1)
        (-3, 1)
    , expect1 "eval" eval
        (Mult (Plus (Number 2 1) (Number (-6) 1)) (Plus (Number 3 1) (Number 2 1)))
        (-20, 1)
    , expect1 "eval" eval
        (Div (Number 13 1) (Number 6 1))
        (13, 6)
    , expect1 "eval" eval
        (Plus (Mult (Number 2 1) (Number 3 1)) (Div (Number 13 1) (Number (-6) 1)))
        (-23, -6)
    , expect1 "eval" eval
        (Plus (Number 10 1) (Number (-14) 1))
        (-4, 1)
    , expect1 "eval" eval
        (Mult (Number 23 1) (Number (-12) 1))
        (-276, 1)
    , expect1 "eval" eval
        (Plus (Number 7 5) (Number 4 3))
        (41, 15)
    , expect1 "tokenize" tokenize
         "1+2"
        [NumTok "1", AddTok, NumTok "2"]
    , expect1 "tokenize" tokenize
        "1 + 20"
        [NumTok "1", AddTok, NumTok "20"]
    , expect1 "tokenize" tokenize
        "1 * -2"
        [NumTok "1", MulTok, NumTok "-2"]
    , expect1 "tokenize" tokenize
        "1 + 2 * 3 + 4"
        [NumTok "1", AddTok, NumTok "2", MulTok, NumTok "3", AddTok, NumTok "4"]
    , expect1 "tokenize" tokenize
        "1 * 2 + 3 + 4"
        [NumTok "1", MulTok, NumTok "2", AddTok, NumTok "3", AddTok, NumTok "4"]
    , expect1 "tokenize" tokenize
        "(1+2)"
        [ParTok [NumTok "1", AddTok, NumTok "2"]]
    , expect1 "tokenize" tokenize
        " (1 + 2 )"
        [ParTok [NumTok "1", AddTok, NumTok "2"]]
    , expect1 "tokenize" tokenize
        "(1 * 2 + 3)"
        [ParTok [NumTok "1", MulTok, NumTok "2", AddTok, NumTok "3"]]
    , expect1 "tokenize" tokenize
        "(-10 + 2/5) * 5"
        [ParTok [NumTok "-10", AddTok, NumTok "2", DivTok, NumTok "5"], MulTok, NumTok "5"]
    , expect1 "tokenize" tokenize
        "5 * (-10 + (2 + 4) * 3)"
        [NumTok "5", MulTok,
        ParTok [NumTok "-10", AddTok, ParTok [NumTok "2", AddTok, NumTok "4"],
        MulTok, NumTok "3"]]
    , expect1 "tokenize" tokenize
        "5 * (-10 + (2 + 4) * 3) * (3 + 2)"
        [NumTok "5", MulTok, ParTok [NumTok "-10", AddTok,
        ParTok [NumTok "2", AddTok, NumTok "4"], MulTok, NumTok "3"],
        MulTok, ParTok [NumTok "3", AddTok, NumTok "2"]]
    , expect1 "parse" parse
        [NumTok "1", AddTok, NumTok "2"]
        (Plus (Number 1 1) (Number 2 1))
    , expect1 "parse" parse
        [NumTok "50", MulTok, NumTok "-76"]
        (Mult (Number 50 1) (Number (-76) 1))
    , expect1 "parse" parse
        [ParTok [NumTok "50", MulTok, NumTok "-76"]]
        (Mult (Number 50 1) (Number (-76) 1))
    , expect1 "parse" parse
        [ParTok [NumTok "2", AddTok, NumTok "4"], MulTok, NumTok "-23"]
        (Mult (Plus (Number 2 1) (Number 4 1)) (Number (-23) 1))
    , expect1 "parse" parse
        [NumTok "4", MulTok, ParTok [NumTok "4", MulTok, ParTok [NumTok "4"]]]
        (Mult (Number 4 1) (Mult (Number 4 1) (Number 4 1)))
    , expect1 "parse" parse
        [NumTok "1", AddTok, NumTok "2", MulTok, NumTok "3", AddTok, NumTok "4"]
        (Plus (Number 1 1) (Plus (Mult (Number 2 1) (Number 3 1)) (Number 4 1)))
    , expect1 "parse" parse
        [NumTok "5", MulTok, ParTok [NumTok "-10", AddTok,
        ParTok [NumTok "2", AddTok, NumTok "4"], MulTok, NumTok "3"],
        MulTok, ParTok [NumTok "3", AddTok, NumTok "2"]]
        (Mult (Number 5 1) (Mult (Plus (Number (-10) 1)
        (Mult (Plus (Number 2 1) (Number 4 1)) (Number 3 1)))
        (Plus (Number 3 1) (Number 2 1))))
    ]



-- Inspect the below in GHCi.

-- DO NOT MODIFY BELOW THIS LINE IN YOUR SUBMISSION --

successes       = filter isSuccess tests
failures        = filter (not . isSuccess) tests
failureMessages = map message failures

results =
    ( length successes
    , length failures
    , failureMessages
    )

showFailures :: IO ()
showFailures = mapM_ putStrLn failureMessages
