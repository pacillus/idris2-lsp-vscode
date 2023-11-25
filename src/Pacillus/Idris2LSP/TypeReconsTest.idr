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
parseSig x with (lexSimpleExpr x)
  parseSig x | Just toks = parseSignature toks
  parseSig x | Nothing = Left "Failed to lex."
  -- case lexSimpleExpr x of
  --   Just toks => parseSignature toks
  --   Nothing => Left "Failed to lex."

test : String -> String -> Either String PartialExprSignature
test str1 str2 =
  let
    maybast1 = parseSig str1
    maybast2 = parseSig str2
  in
    case (maybast1, maybast2) of
        ((Left x), (Left y)) => Left $ "Multiple errors:\n" ++ x ++ "\n" ++ y
        ((Left x), (Right y)) => Left x
        ((Right x), (Left y)) => Left y
        ((Right x), (Right y)) => getAppliedType (toSpSig x) (toSpSig y)




convertInList2ListIn : Monad f => List (f b) -> f (List b)
convertInList2ListIn [] = pure []
convertInList2ListIn (mnd :: mnds) =
  do
    xs <- convertInList2ListIn mnds
    x <- mnd
    pure $ x :: xs


outputTest : String
outputTest =
  let
    target : Either String SimpleExpr 
    target = parse "ExExArr (assignExpr x var replace) (assignExpr y var replace)"
    target2 : Either String SimpleExpr 
    target2 = parse "test003 . test003_1"
    target3 : Either String SimpleExpr
    target3 = parse "xs ++ (y :: ys)"
    sigList =[
      parseSig "ExExArr : SimpleExpr -> SimpleExpr -> Arrow b",
      parseSig "assignExpr : SimpleExpr -> SimpleExpr -> SimpleExpr -> SimpleExpr",
      parseSig "x : SimpleExpr",
      parseSig "var : SimpleExpr",
      parseSig "replace : SimpleExpr",
      parseSig "y : SimpleExpr"
    ]
    sigList2 = [
      parseSig "test003 : Vect n String -> Vect (S n) String",
      parseSig "(.) : (b -> c) -> (a -> b) -> a -> c",
      parseSig "test003_1 : a -> Vect 4 a"
    ]
    sigList3 = [
      parseSig "xs : Vect 4 Nat",
      parseSig "(++) : (xs : Vect m elem) -> (ys : Vect n elem) -> Vect (m + n) elem",
      parseSig "y : Nat",
      parseSig "(::) : elem -> Vect len elem -> Vect (S len) elem",
      parseSig "ys : Vect 2 Nat"
    ]
    result =
      do
        sigs <- convertInList2ListIn sigList3
        tgt <- target3
        getPartialType sigs tgt
  in
  
      case result of
        Left error => error
        Right tree => show tree