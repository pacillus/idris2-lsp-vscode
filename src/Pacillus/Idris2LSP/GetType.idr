module Pacillus.Idris2LSP.GetType

import Data.List
import Data.List1
import Data.String
import Language.JSON
import System
import Text.Parser.Expression

import Pacillus.Idris2LSP.Parser.Basic
import Pacillus.Idris2LSP.Parser.Desugared
import Pacillus.Idris2LSP.Parser.Sugared
import Pacillus.Idris2LSP.TypeTree.TypeTree
import Pacillus.Idris2LSP.TypeTree.Output
import Pacillus.Idris2LSP.Util


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

json2Info : JSON -> Either String (String, InOperatorMap, List String)
json2Info input@(JObject (xs)) =
  do
    x <- findFirst #"Error : Failed to find member "expr" in input JSON"# conv1 xs
    yraw <- findFirst  #"Error : Failed to find member "ops" in input JSON"# conv2 xs
    zraw <- findFirst #"Error : Failed to find member "sigs" in input JSON"# conv3 xs
    y <- convertInList2ListIn $ map json2OpMap yraw
    z <- convertInList2ListIn $ map convstrarr zraw
    pure (x, y, z)
      where
        conv1 : (String, JSON) -> Either String String
        conv1 ("expr", (JString str)) = Right str
        conv1 _ = Left ""
        conv2 : (String, JSON) -> Either String (List JSON)
        conv2 ("ops", (JArray jsons)) = Right jsons
        conv2 _ = Left ""
        conv3 : (String, JSON) -> Either String (List JSON)
        conv3 ("sigs", (JArray jsons)) = Right jsons
        conv3 _ = Left ""
        convstrarr : JSON -> Either String String
        convstrarr (JString str) = Right str
        convstrarr _ = Left #"Error : Non string at "sigs" in input JSON"#
json2Info _ = Left "Invalid input JSON form"

parseInput : String -> Either String (String, InOperatorMap, List String)
parseInput str =
  case Language.JSON.parse str of
    Nothing => Left "Error : Input JSON parse failed"
    (Just x) => json2Info x

-- parseAndDesugarSigs : InOperatorMap -> List String -> Either String (List (Desugared NoHole))
-- parseAndDesugarSigs _ [] = Right []
-- parseAndDesugarSigs opmap (str :: xs) with (parseSig opmap str)
--   parseAndDesugarSigs opmap (str :: xs) | (Left x) = Left x
--   parseAndDesugarSigs opmap (str :: xs) | (Right x) = ?rhs

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

inferType : String -> InOperatorMap -> List String -> String
inferType expr opmap types =
  let
    ty_list = map (parseSig opmap) types
    result = 
      do
        sigs <- convertInList2ListIn ty_list
        target <- parse opmap expr
        des_sigs_pats <- Right $ toPatterns (map desugarSig sigs)
        Right $ [getPartialType des_sigs target' | des_sigs <- des_sigs_pats, target' <- desugar target]
  in
  case result of
    (Left error) => error
    (Right results) => showResults results

process : String -> String
process str =
  case parseInput str of
    (Left err) => err
    (Right info) =>
      let (expr, opmap, types) = info in
        inferType expr opmap (
          "Builtin.DPair.DPair : (a : Type) -> (a -> Type) -> Type" ::
          "Builtin.DPair.MkDPair : (fst : a) -> p fst -> DPair a p" :: 
          "Builtin.Pair : Type -> Type -> Type" ::
          "Builtin.MkPair : a -> b -> Pair a b" ::
          "Builtin.MkUnit : Unit" ::
          "Builtin.Unit : Type" ::
          "Builtin.(===) : a -> a -> Type" ::
          "Prelude.fromInteger : Num ty => Integer -> ty" ::
          "Builtin.fromDouble : FromDouble ty => Double -> ty" ::
          "Builtin.fromChar : FromChar ty => Char -> ty" ::
          "Builtin.fromString : FromString ty => String -> ty" ::
          "String : Type" ::
          "Char : Type" ::
          "Int : Type" ::
          "Integer : Type" ::
          "Double : Type" ::
            types)

main : IO ()
main =
  do
    args <- getArgs 
    case args of
        [] => putStrLn "*Unknown Error : Something went wrong with arguments"
        (execname :: []) => putStrLn "*Error : No input arguments"
        (execname :: (input :: extra)) =>
            putStrLn $ process input