module Pacillus.Idris2LSP.Lex

import Data.List1
import Data.String
import Language.JSON
import System
import Text.Lexer

import Pacillus.Idris2LSP.Parser.Lexer
import Pacillus.Idris2LSP.Util

isNoType : SimpleExprToken -> Bool
isNoType (Tok SESymbol text) = False
isNoType (Tok SEIgnore text) = True
isNoType (Tok SELParen text) = True
isNoType (Tok SERParen text) = True
isNoType (Tok SELBracket _) = True
isNoType (Tok SERBracket _) = True
isNoType (Tok SEIdentifier text) = False
isNoType (Tok SEOperator text) = False
isNoType (Tok SEMember text) = False
isNoType (Tok SEBackquote text) = True
isNoType (Tok SEArrow text) = True
isNoType (Tok SEDoubleArrow text) = True
isNoType (Tok SEBackslash text) = True
isNoType (Tok SEEqual text) = True
isNoType (Tok SEColon text) = True
isNoType (Tok SEComma text) = True
isNoType (Tok SEDollar text) = True
isNoType (Tok SEDoubleStar _) = True
isNoType (Tok SEWildcard _) = True
isNoType (Tok SEIntLiteral text) = True
isNoType (Tok SEDoubleLiteral text) = True
isNoType (Tok SECharLiteral text) = True
isNoType (Tok SEStringLiteral text) = True

absolutePos : (Int, Int) -> (Int, Int) -> (Int, Int)
absolutePos start@(line, start_character) (0, character) = (line, start_character + character)
absolutePos start@(start_line, _) (line, character) = (start_line + line, character)

parseInput : String -> Either String ((Int, Int), String)
parseInput str with (Language.JSON.parse str)
  parseInput str | Nothing = Left "Failed(Lex) : Failed to parse input as JSON"
  parseInput str | (Just (JObject xs)) = 
    let
      startConvert : (String, JSON) -> Either String (Int, Int)
      startConvert ("start", (JObject ys)) = 
        let
          lineConvert : (String, JSON) -> Either String Int
          lineConvert ("line", (JString str1)) with (parseInteger str1)
            lineConvert ("line", (JString str1)) | Nothing = Left ""
            lineConvert ("line", (JString str1)) | (Just x) = Right $ cast x
          lineConvert _ = Left ""
          characterConvert : (String, JSON) -> Either String Int
          characterConvert ("character", (JString str1)) with (parseInteger str1)
            characterConvert ("character", (JString str1)) | Nothing = Left ""
            characterConvert ("character", (JString str1)) | (Just x) = Right $ cast x
          characterConvert _ = Left ""
        in
        do
          line <- findFirst #"Error : Failed to find member "start.line" in input JSON"# lineConvert ys
          character <- findFirst #"Error : Failed to find member "start.line" in input JSON"# characterConvert ys
          Right (line, character)
      startConvert _ = Left ""

      textConvert : (String, JSON) -> Either String String
      textConvert ("text", (JString str')) = Right str'
      textConvert _ = Left ""
    in
    do 
      start <- findFirst #"Error : Failed to find member "start" in input JSON"# startConvert xs
      text <- findFirst #"Error : Failed to find member "text" in input JSON"# textConvert xs
      Right (start, text)
  parseInput str | (Just _) = Left "Failed(Lex) : corrput object; top must be an object"
-- parseInput (JObject xs) = 
--   do
--     ?parseInput_rhs_7
-- parseInput _ = 


getTokPos' : (Int, Int) -> List (WithBounds SimpleExprToken) -> (List (Int, Int), List String)
getTokPos' start [] = ([], [])
getTokPos' start ((MkBounded val@(Tok SESymbol sym) isIrrelevant (MkBounds startLine startCol endLine endCol)) :: xs) =
  if isNoType val
    then getTokPos' start xs
    else
      let
        pr = getTokPos' start xs
      in
        (absolutePos start (startLine, startCol) :: fst pr, sym :: snd pr)
getTokPos' start ((MkBounded val isIrrelevant (MkBounds startLine startCol endLine endCol)) :: xs) =
  if isNoType val
    then getTokPos' start xs
    else
      let
        pr = getTokPos' start xs
      in
        (absolutePos start (startLine, startCol) :: fst pr, snd pr)


getTokPos : (Int, Int) -> List (WithBounds SimpleExprToken) -> (List (Int, Int), List String)
getTokPos start xs =
  let
    pr = getTokPos' start xs
  in
    (fst pr, nub $ snd pr)

output : Maybe (List (Int, Int), List String) -> JSON
output Nothing = JString "*Error : lex failed"
output (Just (x, y)) = 
  let
    convertPos : (Int, Int) -> JSON
    convertPos (line, character) = JObject [("line", JString $ show line), ("character", JString $ show character)]
  in
    (JObject [("pos", JArray $ map convertPos x), ("syms", JArray $ map JString y)])

lexAndOutput : String -> String
lexAndOutput str with (parseInput str)
  lexAndOutput str | (Left err) = err 
  lexAndOutput str | (Right (start, text)) = show $ output $ map (getTokPos start) $ lexSimpleExpr text
-- show $ output $ map getTokPos $ lexSimpleExpr str

main : IO ()
main =
  do
    args <- getArgs
    case args of
        [] => putStrLn "*Unknown Error : Something went wrong with arguments"
        (execname :: args) => putStrLn $ lexAndOutput $ unwords args