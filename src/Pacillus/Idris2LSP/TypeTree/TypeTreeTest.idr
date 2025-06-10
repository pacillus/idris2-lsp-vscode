module Pacillus.Idris2LSP.TypeTree.TypeTreeTest

import Data.List1
import Data.String
import Text.Parser.Expression

import Pacillus.Idris2LSP.Parser.Basic
import Pacillus.Idris2LSP.Parser.Desugared
import Pacillus.Idris2LSP.Parser.Sugared
import Pacillus.Idris2LSP.TypeTree.TypeTree
import Pacillus.Idris2LSP.TypeTree.Output

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

convertInList2ListIn : Monad f => List (f b) -> f (List b)
convertInList2ListIn [] = pure []
convertInList2ListIn (mnd :: mnds) =
  do
    xs <- convertInList2ListIn mnds
    x <- mnd
    pure $ x :: xs

data TestCase : Type where
    MkCase : (expr : String) -> (types : List String) -> TestCase

toPatterns : List (List1 (DesugaredSignature NoHole)) -> List1 (List (DesugaredSignature NoHole))
toPatterns [] = [] ::: []
toPatterns (xs :: xss) = [x :: pat | x <- xs, pat <- toPatterns xss]

showResultsMain : List String -> List String -> List (Either String TypeTree) -> String
showResultsMain fails [] [] = unlines ("Derivation failed; Below are the errors" :: fails)
showResultsMain _ succs@(_ :: _) [] = unlines succs
showResultsMain fails succs (Left x :: xs) = showResultsMain (x :: fails) succs xs
showResultsMain fails succs (Right x :: xs) = showResultsMain fails (output x :: succs) xs

showResults : List1 (Either String TypeTree) -> String
showResults = showResultsMain [] [] . forget

testSingleCase : TestCase -> String
testSingleCase (MkCase expr types) =
  let
    ty_list = map (parseSig opMap) types
    sigs = convertInList2ListIn ty_list
    result = 
      do
        sigs <- convertInList2ListIn ty_list
        target <- parse opMap expr
        des_sigs_pats <- Right $ toPatterns (map desugarSig sigs)
        Right $ [getPartialType des_sigs target' | des_sigs <- des_sigs_pats, target' <- desugar target]
  in
  case result of
    (Left error) => error
    (Right results) => showResults results

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
        ],
        MkCase "q p" [
          "p : (x : Nat) -> f x",
          "q : ((y : Nat) -> f y) -> String"
        ],
        MkCase "reverse . decorate . reverse" [
          "reverse : List a -> List a",
          "(.) : (b -> c) -> (a -> b) -> a -> c",
          "decorate : List String -> List String"
        ],
        MkCase "(srjf ** (MkInverse finv invf))" [
          "MkDPair : {p : a -> Type} -> (fst : a) -> p fst -> DPair a p",
          "MkInverse : {g : b -> a} -> ((x : a) -> g (f x) = x) -> ((y : b) -> f (g y) = y) -> Inverse f g",
          "finv : (x : A) -> srjf (f x) = x",
          "srjf : B -> A",
          "invf : (y : B) -> f (srjf y) = y"
        ],
        MkCase "MkDPair srjf" [
          "MkDPair :{p : a -> Type} -> (fst : a) -> p fst -> DPair a p",
          "srjf : B -> A"
        ],
        MkCase "(\\x => x) 10" [

        ],
        MkCase "MkDPair (\\y => fst (prfs y))" [
          "Builtin.DPair.MkDPair : (fst : a) -> p fst -> DPair a p",
          "Builtin.DPair.DPair.fst : DPair a p -> a",
          "prfs : (y : b) -> (x : a ** f x = y)"
        ],
        MkCase "(\\x => prfi (fst (prfs (f x))) x (snd (prfs (f x))))" [
          "prfi : (x : a) -> (y : a) -> f x = f y -> x = y",
          "Builtin.DPair.DPair.fst : DPair a p -> a",
          "prfs : (y : b) -> (x' : a ** f x' = y)",
          "f : a -> b",
          "Builtin.DPair.DPair.snd : (rec : DPair a p) -> p (fst rec)"
        ],
        MkCase "\\y => snd (prfs (f y))" [
          "prfi : (x : a) -> (y : a) -> f x = f y -> x = y",
          "Builtin.DPair.DPair.fst : DPair a p -> a",
          "prfs : (y : b) -> (x' : a ** f x' = y)",
          "f : a -> b",
          "Builtin.DPair.DPair.snd : (rec : DPair a p) -> p (fst rec)"
        ],
        MkCase "snd (prfs (f x))" [
          "prfi : (x : a) -> (y : a) -> f x = f y -> x = y",
          "Builtin.DPair.DPair.fst : DPair a p -> a",
          "prfs : (y : b) -> (x' : a ** f x' = y)",
          "f : a -> b",
          "Builtin.DPair.DPair.snd : (rec : DPair a p) -> p (fst rec)",
          "x : _"
        ],
        MkCase "\\x_0 => prfi (fst (prfs (f x_0))) x_0" [
          "prfi : (x_1 : a) -> (y : a) -> f x_1 = f y -> x_1 = y",
          "Builtin.DPair.DPair.fst : DPair a p -> a",
          "prfs : (y : b) -> (x_2 : a ** f x_2 = y)",
          "f : a -> b",
          "Builtin.DPair.DPair.snd : (rec : DPair a p) -> p (fst rec)"
        ],
        MkCase "prfi (fst (prfs (f x))) x (snd (prfs (f x)))" [
          "prfi : (x : a) -> (y : a) -> f x = f y -> x = y",
          "Builtin.DPair.DPair.fst : DPair a p -> a",
          "prfs : (y : b) -> (x : a ** f x = y)",
          "f : a -> b",
          "Builtin.DPair.DPair.snd : (rec : DPair a p) -> p (fst rec)",
          "x : _"
        ],
        MkCase "MkInverse f (\\y => fst (prfs y))" [
          "prfs : (y : b) -> (x : a ** f x = y)",
          "Builtin.DPair.DPair.fst : DPair a p -> a",
          "Main.MkInverse : (f : (a -> b)) -> (g : (b -> a)) -> ((x : a) -> g (f x) = x) -> ((y : b) -> f (g y) = y) -> Inverse f g",
          "f : a -> b"
        ],
        MkCase "((_ ** Refl), (_ ** Refl))" [
          "Builtin.Refl : x = x",
          "Builtin.Pair : Type -> Type -> Type",
          "Builtin.MkPair : a -> b -> Pair a b",
          "Builtin.DPair.MkDPair : (fst : a) -> p fst -> DPair a p"
        ],
        MkCase "()" [
          "Builtin.MkUnit : Unit",
          "Builtin.Unit : Type"
        ],
        MkCase "(.fun) x.y .z" [
          "x : String",
          "(.y) : String -> Nat",
          "(.z) : Nat -> Char",
          "(.fun) : Char -> Type"
        ]
        -- 課題:暗黙の引数の情報を明記する必要がある場合がある

    ]

test : IO ()
test = putStrLn (testAllCase testCases)
