module Pacillus.Idris2LSP.TypeRecons.TypeReconsTest

import Data.String
import Text.Parser.Expression

import Pacillus.Idris2LSP.Syntax.SimpleExpr
import Pacillus.Idris2LSP.TypeRecons.TypeRecons

opMap : InOperatorMap
opMap = 
    [
        MkOpRecord "$" 0 AssocRight,
        MkOpRecord "+" 8 AssocLeft, 
        MkOpRecord "*" 9 AssocLeft, 
        MkOpRecord "===" 6 AssocNone,
        MkOpRecord "++" 7 AssocRight,
        MkOpRecord "++" 7 AssocRight,
        MkOpRecord "++" 7 AssocRight,
        MkOpRecord ">=" 6 AssocNone,
        MkOpRecord "::" 7 AssocRight,
        MkOpRecord "." 9 AssocRight
    ]





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
    target = parse opMap expr
    ty_list = map (parseSig opMap) types
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
        ],
        MkCase "(vec1 ++ vec2) ++ (vec3 ++ vec4)" [
          "Main.vec1 : Vect 1 Nat",
          "Data.Vect.(++) : Vect m elem -> Vect n elem -> Vect (m + n) elem",
          "Main.vec2 : Vect 2 Nat",
          "Data.Vect.(++) : Vect m elem -> Vect n elem -> Vect (m + n) elem",
          "Main.vec3 : Vect 3 Nat",
          "Data.Vect.(++) : Vect m elem -> Vect n elem -> Vect (m + n) elem",
          "Main.vec4 : Vect 4 Nat"
        ],
        MkCase "f $ g k" [
          "f : x = y -> S x = S y",
          "g : (n : Nat) -> n = n",
          "k : Nat"
        ],
        MkCase "f x" [
          "f : Int -> Int",
          "x : FromString a => a"
        ],
        MkCase "f x" [
          "f : (FromString a => a -> a) -> Int",
          "x : String -> String"
        ], -- TODO fix this
        MkCase "f x" [
          "f : (FromString a => a -> a) -> Int",
          "x : FromString String => String -> String"
        ], -- TODO fix this
        MkCase "f x" [
          "f : (res : Cond x) => res = res -> F res",
          "x : K = K"
        ],
        MkCase "f x y" [
          "f : res -> (res : Cond x) => res = res -> F res",
          "x : Type",
          "y : K = K"
        ],
        MkCase "f x" [
          "f : Int -> Int",
          "x : {a : Type} -> a"
        ]
    ]



test : IO ()
test = putStr (testAllCase testCases)

0 Prop : Nat -> Type

argex : Nat -> Type
argex a =
  let
    sub : Prop a -> Prop a
    sub x = ?hole
  in
    ?hole2 $ sub (?hole1 Prelude.List.reverse)

sum : List Nat
sum = (++) ((::) 2 [3, 4]) [5, 6]

-- | sub : Prop a -> Prop a
-- | ?hole1 : Prop a
-- ------
-- sub ?hole1 : Prop ?a

-- reverse : List a -> List a