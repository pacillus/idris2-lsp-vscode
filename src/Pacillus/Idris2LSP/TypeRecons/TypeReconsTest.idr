module Pacillus.Idris2LSP.TypeRecons.TypeReconsTest

import Data.String

import Pacillus.Idris2LSP.Syntax.SimpleExpr
import Pacillus.Idris2LSP.TypeRecons



  -- case lexSimpleExpr x of
  --   Just toks => parseSignature toks
  --   Nothing => Left "Failed to lex."

-- test : String -> String -> Either String PartialExprSignature
-- test str1 str2 =
--   let
--     maybast1 = parseSig str1
--     maybast2 = parseSig str2
--   in
--     case (maybast1, maybast2) of
--         ((Left x), (Left y)) => Left $ "Multiple errors:\n" ++ x ++ "\n" ++ y
--         ((Left x), (Right y)) => Left x
--         ((Right x), (Left y)) => Left y
--         ((Right x), (Right y)) => getAppliedType (toSpSig x) (toSpSig y)




convertInList2ListIn : Monad f => List (f b) -> f (List b)
convertInList2ListIn [] = pure []
convertInList2ListIn (mnd :: mnds) =
  do
    xs <- convertInList2ListIn mnds
    x <- mnd
    pure $ x :: xs

data TestCase : Type where
    MkCase : (expr : String) -> (types : List String) -> TestCase

testSingleCase : TestCase -> String
testSingleCase (MkCase expr types) =
  let
    target = parse expr
    ty_list = map parseSig types
    result =
      do
        sigs <- convertInList2ListIn ty_list
        tgt <- target
        getPartialType sigs tgt
  in 
    case result of
        Left error => error
        Right tree => show tree

testAllCase : List TestCase -> String
testAllCase xs =
  let
    outputs = map testSingleCase xs
    decorate : List String -> List String
    decorate [] = []
    decorate (str :: strs) = ("case" ++ (show $ S $ length strs) ++ ":\n" ++ str ++ "\n") :: decorate strs
    decorate' : List String -> List String
    decorate' = (reverse . decorate . reverse)

  in trim $ foldl (\x,y => (x ++ "\n\n" ++ y)) "" $ decorate' outputs

testCases : List TestCase
testCases = 
    [
        MkCase "ExExArr (assignExpr x var replace) (assignExpr y var replace)" [
            "ExExArr : SimpleExpr -> SimpleExpr -> Arrow b",
            "assignExpr : SimpleExpr -> SimpleExpr -> SimpleExpr -> SimpleExpr",
            "x : SimpleExpr",
            "var : SimpleExpr",
            "replace : SimpleExpr",
            "y : SimpleExpr"
        ],
        MkCase "test003 . test003_1" [
            "test003 : Vect n String -> Vect (S n) String",
            "(.) : (b -> c) -> (a -> b) -> a -> c",
            "test003_1 : a -> Vect 4 a"            
        ],
        MkCase "xs ++ (y :: ys)" [
            "xs : Vect 4 Nat",
            "(++) : (xs : Vect m elem) -> (ys : Vect n elem) -> Vect (m + n) elem",
            "y : Nat",
            "(::) : elem -> Vect len elem -> Vect (S len) elem",
            "ys : Vect 2 Nat"
        ],
        MkCase "f x" [
            "f : (a : Type) -> List a",
            "x : Type"
        ],
        MkCase "RotateVec.onePlusNEqualNPlus0ne k" [
          "RotateVec.onePlusNEqualNPlus0ne:(n:Nat)->1+n=n+1",
          "k : Nat"
        ],
        MkCase "f x " [
          "f : (a, b) -> a -> b",
          "x : (String, Nat)"
        ]
    ]


test : IO ()
test = putStrLn $ testAllCase testCases