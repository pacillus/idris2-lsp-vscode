module Pacillus.Idris2LSP.GetType

import Data.List
import Data.String
import Language.JSON
import System
import Text.Parser.Expression

import Pacillus.Idris2LSP.Syntax.SimpleExpr
import Pacillus.Idris2LSP.TypeRecons.TypeRecons

convertInList2ListIn : Monad f => List (f b) -> f (List b)
convertInList2ListIn [] = pure []
convertInList2ListIn (mnd :: mnds) =
  do
    x <- mnd
    xs <- convertInList2ListIn mnds
    pure $ x :: xs

findFirst : String -> (a -> Either String b) -> List a -> Either String b
findFirst errtxt f [] = Left errtxt
findFirst errtxt f (x :: xs) = either (const $ findFirst errtxt f xs) Right (f x)

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
        assocconv "right" = Right AssocRight
        assocconv "left" = Right AssocLeft
        assocconv "none" = Right AssocNone
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



inferType : String -> InOperatorMap -> List String -> String
inferType expr opmap types =
  let
    target = (parse opmap) expr
    ty_list = map (parseSig opmap) types
    result = 
      do
        sigs <- convertInList2ListIn ty_list
        target >>= getPartialType sigs
  in
  case result of
    (Left error) => error
    (Right tree) => show tree

process : String -> Either String String
process str =
  do
    info <- parseInput str
    let (expr, opmap, types) = info
    pure $ inferType expr opmap types

main : IO ()
main =
  do
    args <- getArgs 
    case args of
        [] => putStrLn "*Unknown Error : Something went wrong with arguments"
        (execname :: []) => putStrLn "*Error : No input arguments"
        (execname :: (input :: extra)) =>
            putStrLn $ either id id (process input)