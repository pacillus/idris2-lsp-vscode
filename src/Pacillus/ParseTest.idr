module Pacillus.ParseTest

import Pacillus.Idris2LSP.Syntax.SimpleExpr

testPatternch01 : List String
testPatternch01 =
    [
        "Bool -> Type",
        "Int",
        "String",
        "(x : Bool) -> StringOrInt x",
        "94",
        #""Ninety four""#,
        "(x : Bool) -> StringOrInt x -> String",
        "cast val",
        "val"
    ]

testPatternch02 : List String
testPatternch02 =
    [
        #""The average word length is: " ++ show (average str) ++ "\n""#,
        #"repl "Enter a string: " showAverage"#
    ]

-- parseSmpl : String -> Either String Expr
-- parseSmpl = parse

parse_list : List String -> List (Either String Expr)
parse_list xs = map parse xs

refineResult' : Nat -> List (Either String Expr) -> Either (List Expr) (String, Nat)
refineResult' n [] = Left []
refineResult' n ((Left x) :: xs) = Right (x, n)
refineResult' n ((Right x) :: xs) =
    case refineResult' (S n) xs of
        Left y => Left (x :: y)
        Right fl => Right fl

refineResult : List (Either String Expr) -> Either (List Expr) (String, Nat)
refineResult xs = refineResult' 0 xs

test : Either (List Expr) (String, Nat)
test = refineResult $ parse_list testPatternch02