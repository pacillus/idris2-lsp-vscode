module Pacillus.Idris2LSP.Lex

import Data.List1
import Data.String
import Language.JSON
import System
import Text.Lexer

import Pacillus.Idris2LSP.Syntax.Lexer

isNoType : SimpleExprToken -> Bool
isNoType (Tok SEDollar text) = True
isNoType (Tok SESymbol text) = False
isNoType (Tok SEIgnore text) = True
isNoType (Tok SELParen text) = True
isNoType (Tok SERParen text) = True
isNoType (Tok SEIdentifier text) = False
isNoType (Tok SEBackquote text) = True
isNoType (Tok SEArrow text) = True
isNoType (Tok SEDoubleArrow text) = True
isNoType (Tok SEEqual text) = True
isNoType (Tok SEColon text) = True
isNoType (Tok SEComma text) = True
isNoType (Tok SEIntLiteral text) = True
isNoType (Tok SEDoubleLiteral text) = True
isNoType (Tok SECharLiteral text) = True
isNoType (Tok SEStringLiteral text) = True


getTokPos' : List (WithBounds SimpleExprToken) -> (List Int, List String)
getTokPos' [] = ([], [])
getTokPos' ((MkBounded val@(Tok SESymbol sym) isIrrelevant (MkBounds startLine startCol endLine endCol)) :: xs) =
  if isNoType val
    then getTokPos' xs
    else
      let
        pr = getTokPos' xs
      in
        (startCol :: fst pr, sym :: snd pr)
getTokPos' ((MkBounded val isIrrelevant (MkBounds startLine startCol endLine endCol)) :: xs) =
  if isNoType val
    then getTokPos' xs
    else
      let
        pr = getTokPos' xs
      in
        (startCol :: fst pr, snd pr)


getTokPos : List (WithBounds SimpleExprToken) -> (List Int, List String)
getTokPos xs =
  let
    pr = getTokPos' xs
  in
    (fst pr, nub $ snd pr)

output : Maybe (List Int, List String) -> JSON
output Nothing = JString "*Error : lex failed"
output (Just (x, y)) = (JObject [("pos", JArray $ map (JString . show) x), ("syms", JArray $ map JString y)])

lexAndOutput : String -> String
lexAndOutput str = show $ output $ map getTokPos $ lexSimpleExpr str

main : IO ()
main =
  do
    args <- getArgs
    case args of
        [] => putStrLn "*Unknown Error : Something went wrong with arguments"
        (execname :: args) => putStrLn $ lexAndOutput $ unwords args