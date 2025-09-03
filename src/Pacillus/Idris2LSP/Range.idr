module Pacillus.Idris2LSP.Range

import Data.List1
import Data.Nat
import Data.String
import Language.JSON
import System
import Text.Parser.Expression

import Pacillus.Idris2LSP.Parser.Basic
import Pacillus.Idris2LSP.Parser.Sugared
import Pacillus.Idris2LSP.Util
-- "ops" : [{"symbol" : "$", "prec" : "0", "assoc" : "infixr"}, ...}], "target" : "f $ x"

Range : Type
Range = (Nat, Nat)

data RangeTree = Atom Range | Compound Range (List1 RangeTree)

appToAll : (Nat -> Nat) -> RangeTree -> RangeTree
appToAll f (Atom (s, e)) = Atom $ (f s, f e)
appToAll f (Compound (s, e) (tree_h ::: tree_t)) =
  let
    sub : List RangeTree -> List RangeTree
    sub [] = []
    sub (x :: xs) = appToAll f x :: sub xs
  in
    Compound (f s,  f e) $ appToAll f tree_h ::: sub tree_t

convertInList2ListIn : Monad f => List (f b) -> f (List b)
convertInList2ListIn [] = pure []
convertInList2ListIn (mnd :: mnds) =
  do
    x <- mnd
    xs <- convertInList2ListIn mnds
    pure $ x :: xs

json2OpMap : JSON -> Either String OpRecord
json2OpMap (JObject xs) =
  do
    symbol <- findFirst #"Error : Failed to find member "ops.symbol" in input JSON"# conv1 xs
    precraw <- findFirst #"Error : Failed to find member "ops.prec" in input JSON"# conv2 xs
    assocraw <- findFirst #"Error : Failed to find member "ops.assoc" in input JSON"# conv3 xs
    prec <- maybe (Left #"Error : Found none number at "ops.prec" in input JSON"#) Right (map (fromInteger {ty = Nat}) $ parseInteger precraw)
    assoc <- assocconv assocraw
    pure $ MkOpRecord symbol prec assoc
      where
        conv1 : (String, JSON) -> Either String String
        conv1 ("symbol", (JString str)) = Right str
        conv1 _ = Left ""
        conv2 : (String, JSON) -> Either String String
        conv2 ("prec", (JString str)) = Right str
        conv2 _ = Left ""
        conv3 : (String, JSON) -> Either String String
        conv3 ("assoc", (JString str)) = Right str
        conv3 _ = Left ""
        assocconv : String -> Either String Assoc
        assocconv "infixr" = Right AssocRight
        assocconv "infixl" = Right AssocLeft
        assocconv "infix" = Right AssocNone
        assocconv _ = Left #"Error : Invalid value at "ops.assoc" in input JSON"#
json2OpMap _ = Left #"Invalid JSON format at "ops" in input JSON "#

json2Info : JSON -> Either String (String, InOperatorMap)
json2Info input@(JObject (xs)) =
  do
    x <- findFirst #"Error : Failed to find member "expr" in input JSON"# conv1 xs
    yraw <- findFirst  #"Error : Failed to find member "ops" in input JSON"# conv2 xs
    y <- convertInList2ListIn $ map json2OpMap yraw
    pure (x, y)
      where
        conv1 : (String, JSON) -> Either String String
        conv1 ("expr", (JString str)) = Right str
        conv1 _ = Left ""
        conv2 : (String, JSON) -> Either String (List JSON)
        conv2 ("ops", (JArray jsons)) = Right jsons
        conv2 _ = Left ""
json2Info _ = Left "Invalid input JSON form"

parseInput : String -> Either String (String, InOperatorMap)
parseInput str =
  case Language.JSON.parse str of
    Nothing => Left "Error : Input JSON parse failed"
    (Just x) => json2Info x

countToken : Sugared Expr -> (n : Nat ** m : Nat ** n = S m)
countToken (IdentifierTerm x) = (1 ** _ ** Refl)
countToken (Application e1 e2) = 
    let
        ((S n1) ** _ ** Refl) = countToken e1
            | (Z ** _ ** prf) => absurd prf
        (n2@(S _) ** _ ** Refl) = countToken e2
            | (Z ** x ** prf) => absurd prf
    in
        (S n1 + n2 ** _ ** Refl)
countToken (Arrow _ e1 e2) = (S $ (countToken e1).fst + (countToken e2).fst ** _ ** Refl)
countToken (SignatureArrow _ _ e1 e2) = 
    (5 + (countToken e1).fst + (countToken e2).fst ** _ ** Refl)
countToken (BracketArrow _ e1 e2) = (5 + (countToken e1).fst + (countToken e2).fst ** _ ** Refl)
countToken (AnonymousFunction _ y) = (3 + (countToken y).fst ** _ ** Refl)
countToken (Literal t x) = (1 ** _ ** Refl)
countToken (Parenthesis e) = (2 + (countToken e).fst ** _ ** Refl)
countToken Wildcard = (1 ** _ ** Refl)
countToken (HoleTerm x) = (1 ** _ ** Refl)
countToken (RewriteIn prf e) = (2 + (countToken prf).fst + (countToken e).fst ** _ ** Refl)
countToken UnitSugar = (1 ** _ ** Refl)
countToken (PairSugar e1 e2 es) = 
    let
        es_count = foldl (\acc, e => acc + (countToken e).fst) 0 es
    in
        (3 + length es + (countToken e1).fst + (countToken e2).fst + es_count ** _ ** Refl)
countToken (OpInfixSugar e1 _ e2) = (S ((countToken e1).fst + (countToken e2).fst) ** _ ** Refl)
countToken (InfixSugar e1 _ e2) = (3 + ((countToken e1).fst + (countToken e2).fst) ** _ ** Refl)
countToken (DependentPairSugar _ e1 e2) = 
    (5 + (countToken e1).fst + (countToken e2).fst ** _ ** Refl)
countToken (DependentPairConstructorSugar e1 e2) = (3 + (countToken e1).fst + (countToken e2).fst ** _ ** Refl)
countToken (EqualSugar e1 e2) = (S ((countToken e1).fst + (countToken e2).fst) ** _ ** Refl)
countToken (MemberSugar x _) = (S (countToken x).fst ** _ ** Refl)
countToken (DollarSugar e1 e2) = (S ((countToken e1).fst + (countToken e2).fst) ** _ ** Refl)

-- returns [(start_index, end_index + 1)...]
getTokenRange : Sugared Expr -> RangeTree
getTokenRange (IdentifierTerm x) = Atom (0, 1)
getTokenRange e@(Application e1 e2) = -- e1 e2
    let
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** _ ** prf) => absurd prf
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
    in
        Compound (0, S end) $ getTokenRange e1 ::: [appToAll (+ n1) (getTokenRange e2)]
getTokenRange e@(Arrow _ e1 e2) = 
    let
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** x ** prf) => absurd prf
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
    in
        Compound (0, S end) $ getTokenRange e1 ::: [appToAll (1 + n1 +) (getTokenRange e2)]
getTokenRange e@(SignatureArrow _ _ e1 e2) = 
    let
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** x ** prf) => absurd prf
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
    in
        -- (x : Nat) -> Type
        Compound (0, S end) $ appToAll (3 +) (getTokenRange e1) ::: [appToAll (5 + n1 +) (getTokenRange e2)]
getTokenRange e@(BracketArrow _ e1 e2) = 
    let
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** x ** prf) => absurd prf
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
    in
        -- {x : Type} -> Type
        Compound (0, S end) $ appToAll (+ 3) (getTokenRange e1) ::: [appToAll (5 + n1 +) (getTokenRange e2)]
getTokenRange e@(AnonymousFunction _ e1) = 
    let
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
    in
        -- \x => e
        Compound (0, S end) $ appToAll (3 +) (getTokenRange e1) ::: []
getTokenRange (Literal _ _) = Atom (0, 1)
getTokenRange e@(Parenthesis e1) = 
    let
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
    in
        appToAll (1 +) (getTokenRange e1)
getTokenRange Wildcard = Atom (0, 1)
getTokenRange (HoleTerm x) = Atom (0, 1)
getTokenRange e@(RewriteIn prf e1) = 
    let
        (S end ** end ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
        (nprf@(S _) ** _ ** Refl) = countToken prf
            | (Z ** _ ** prf) => absurd prf
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** _ ** prf) => absurd prf
    in
        Compound (0, S end) $ appToAll (1 +) (getTokenRange prf) ::: [appToAll (2 + nprf +) (getTokenRange e1)]
getTokenRange UnitSugar = Atom (0, 1)
getTokenRange e@(PairSugar e1 e2 es) = 
    let
        (S end ** end ** Refl) = countToken e
            | (Z ** n ** prf) => absurd prf
        (n1@(S n1m1) ** _ ** Refl) = countToken e1
            | (Z ** x ** prf) => absurd prf
        (n2@(S n1m2) ** _ ** Refl) = countToken e2
            | (Z ** x ** prf) => absurd prf
        getTokenRangeOfEs : Nat -> List (Sugared Expr) -> List RangeTree
        getTokenRangeOfEs k [] = []
        getTokenRangeOfEs k (e :: es) = 
            let
                (n@(S _) ** _ ** Refl) = countToken e
                    | (Z ** x ** prf) => absurd prf
            in
                appToAll (k +) (getTokenRange e) :: getTokenRangeOfEs (1 + k + n) es
    in
        --(a, b, c)
        
        Compound (0, S end) $
            appToAll (1 +) (getTokenRange e1)
            ::: 
            appToAll (2 + n1 +) (getTokenRange e2) :: 
              getTokenRangeOfEs (3 + n1 + n2) es
getTokenRange e@(OpInfixSugar e1 _ e2) = 
    let
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** _ ** prf) => absurd prf
    in
        Compound (0, S end) $ getTokenRange e1 ::: [appToAll (S n1 +) (getTokenRange e2)]
getTokenRange e@(InfixSugar e1 _ e2) = 
    let
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** _ ** prf) => absurd prf
    in
        Compound (0, S end) $ getTokenRange e1 ::: [appToAll (3 + n1 +) (getTokenRange e2)]
getTokenRange e@(DependentPairSugar _ e1 e2) = 
    let
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** _ ** prf) => absurd prf
    in
        Compound (0, S end) $ appToAll (3 +) (getTokenRange e1) ::: [appToAll (4 + n1 +) (getTokenRange e2)]
getTokenRange e@(DependentPairConstructorSugar e1 e2) = 
    let
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** x ** prf) => absurd prf
    in
        Compound (0, S end) $ appToAll (1 +) (getTokenRange e1) ::: [appToAll (2 + n1 +) (getTokenRange e2)]
getTokenRange e@(EqualSugar e1 e2) = 
    let
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** x ** prf) => absurd prf
    in
        Compound (0, S end) $ getTokenRange e1 ::: [appToAll (1 + n1 +) (getTokenRange e2)]
getTokenRange e@(MemberSugar e1 _) = 
    let
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
    in
        Compound (0, S end) $ getTokenRange e1 ::: []
getTokenRange e@(DollarSugar e1 e2) = 
    let
        (S end ** _ ** Refl) = countToken e
            | (Z ** _ ** prf) => absurd prf
        (n1@(S _) ** _ ** Refl) = countToken e1
            | (Z ** x ** prf) => absurd prf
    in
        Compound (0, S end) $ getTokenRange e1 ::: [appToAll (1 + n1 +) (getTokenRange e2)]

outputRange : Range -> JSON
outputRange (x, y) = JObject [("start", JNumber $ cast x), ("end", JNumber $ cast y)]

output : RangeTree -> JSON
output (Atom r) = JObject [("value", outputRange r), ("tag", JString "atom")]
output (Compound r trees) = 
  let
    branches = ("branches", JArray $ map output $ forget trees)
  in
    JObject [("value", outputRange r), ("tag", JString "compound"), branches]

countTokenOfExpr : String -> InOperatorMap -> String
countTokenOfExpr expr opmap = 
    case parse opmap expr of
        (Left err) => err
        (Right x) => show $ output (getTokenRange x)

process : String -> String
process str = 
    case parseInput str of
        (Left err) => err
        (Right (expr, opmap)) => countTokenOfExpr expr opmap

main : IO ()
main =
  do
    args <- getArgs
    case args of
        [] => putStrLn "*Unknown Error : Something went wrong with arguments"
        (execname :: args) => putStrLn $ process $ unwords args
