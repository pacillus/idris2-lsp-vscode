module Pacillus.Idris2LSP.Lex

import Data.List1
import Data.String
import Language.JSON
import System
import Text.Lexer

import Pacillus.Idris2LSP.Syntax.Lexer




getTokPos' : List (WithBounds SimpleExprToken) -> (List Int, List String)
getTokPos' [] = ([], [])
getTokPos' ((MkBounded (Tok SEIgnore text) isIrrelevant (MkBounds startLine startCol endLine endCol)) :: xs) =
    getTokPos' xs
getTokPos' ((MkBounded (Tok SESymbol sym) isIrrelevant (MkBounds startLine startCol endLine endCol)) :: xs) =
  let
    pr = getTokPos' xs
  in
    (startCol :: fst pr, sym :: snd pr)
getTokPos' ((MkBounded _ isIrrelevant (MkBounds startLine startCol endLine endCol)) :: xs) =
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
lexAndOutput str = show $ output $ map getTokPos' $ lexSimpleExpr str

main : IO ()
main =
  do
    args <- getArgs
    case args of
        [] => putStrLn "*Unknown Error : Something went wrong with arguments"
        (execname :: args) => putStrLn $ lexAndOutput $ unwords args