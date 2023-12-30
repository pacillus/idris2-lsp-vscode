module Pacillus.Idris2LSP.Syntax.ParseTest

import Data.List
import Data.String
import Data.Zippable
import Text.Parser.Expression

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
        "val",
        "Prelude.(+)"
    ]

testPatternch02 : List String
testPatternch02 =
    [
        #""The average word length is: " ++ show (average str) ++ "\n""#,
        #"repl "Enter a string: " showAverage"#
    ]

-- mytestPattern : List String

-- [((IdTerm (MkId "x")), (IdTerm (MkId "True")))]

-- unify [((AppTerm (MkApp (AppTerm (MkApp (IdTerm (MkId "Vect")) (IdTerm (MkId "n")))) (IdTerm (MkId "ty")))), (AppTerm (MkApp (AppTerm (MkApp (IdTerm (MkId "Vect")) (NatLiteral 4))) (IdTerm (MkId "String")))))]

-- (labelImplicit . nonImplicitList) (ArwTerm (SiExArr (MkSignature "x" (IdTerm (MkId "Bool"))) (AppTerm (MkApp (IdTerm (MkId "StringOrInt")) (IdTerm (MkId "x"))))))

-- parseSmpl : String -> Either String Expr
-- parseSmpl = parse

opMap : InOperatorMap
opMap = 
    [
        MkOpRecord "$" 0 AssocRight,
        MkOpRecord "+" 8 AssocLeft, 
        MkOpRecord "*" 9 AssocLeft, 
        MkOpRecord "===" 6 AssocNone,
        MkOpRecord "++" 7 AssocRight,
        MkOpRecord ">=" 6 AssocNone,
        MkOpRecord "::" 7 AssocRight,
        MkOpRecord "." 9 AssocRight
    ]

parse_list : List String -> List (Either String SimpleExpr)
parse_list xs = map (parse opMap) xs

refineResult' : Nat -> List (Either String SimpleExpr) -> Either (List SimpleExpr) (String, Nat)
refineResult' n [] = Left []
refineResult' n ((Left x) :: xs) = Right (x, n)
refineResult' n ((Right x) :: xs) =
    case refineResult' (S n) xs of
        Left y => Left (x :: y)
        Right fl => Right fl

refineResult : List (Either String SimpleExpr) -> Either (List SimpleExpr) (String, Nat)
refineResult xs = refineResult' 0 xs

test : Either (List SimpleExpr) (String, Nat)
test = refineResult $ parse_list testPatternch01


isSuccess : Either String SimpleExpr -> String
isSuccess (Left x) = ":X"
isSuccess (Right x) = ":O"

parseNListResult : List String -> IO ()
parseNListResult xs =
    let
        parsed = map (parse opMap) xs
        results = map isSuccess parsed
        output = zipWith (++) xs results
    in 
        putStrLn $ unlines output