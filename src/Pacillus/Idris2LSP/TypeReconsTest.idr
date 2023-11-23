module Pacillus.Idris2LSP.TypeReconsTest

import Text.Parser
import Text.Parser.Expression

import Pacillus.Idris2LSP.Syntax.SimpleExpr
import Pacillus.Idris2LSP.Syntax.Lexer
import Pacillus.Idris2LSP.TypeRecons


parseSignature : List (WithBounds SimpleExprToken) -> Either String Signature
parseSignature toks =
  case parse (signature opTable) $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left "contains tokens that were not consumed"
    Left e => Left (show e)

parseSig : String -> Either String Signature
parseSig x =
  case lexSimpleExpr x of
    Just toks => parseSignature toks
    Nothing => Left "Failed to lex."

test : String -> String -> Either String Signature
test str1 str2 =
  let
    maybast1 = parseSig str1
    maybast2 = parseSig str2
  in
    case (maybast1, maybast2) of
        ((Left x), (Left y)) => Left $ "Multiple errors:\n" ++ x ++ "\n" ++ y
        ((Left x), (Right y)) => Left x
        ((Right x), (Left y)) => Left y
        ((Right x), (Right y)) => getAppliedType x y