module Pacillus.Idris2LSP.Syntax.Lexer

import Text.Lexer
import Data.Maybe
import Data.String

-- data to identify tokens
public export
data SimpleExprTokenKind =
      SESymbol
    | SEIgnore
    | SELParen
    | SERParen
    | SELBracket
    | SERBracket
    | SEIdentifier
    | SEBackquote
    | SEArrow
    | SEDoubleArrow
    | SEEqual
    | SEColon
    | SEComma
    | SEDollar
    | SEIntLiteral
    | SECharLiteral
    | SEDoubleLiteral
    | SEStringLiteral


-- normal implementation.
-- DO NOT FORGET TO ADD DEFENITON FOR NEW SYMBOL
-- IT IS SET TO FALSE BY DEFAULT AND IT IS EASY TO MISS
export
Eq SimpleExprTokenKind where
  (==) SESymbol SESymbol = True
  (==) SEIgnore SEIgnore = True
  (==) SELParen SELParen = True
  (==) SERParen SERParen = True
  (==) SELBracket SELBracket = True
  (==) SERBracket SERBracket = True
  (==) SEIdentifier SEIdentifier = True
  (==) SEBackquote SEBackquote = True
  (==) SEArrow SEArrow = True
  (==) SEDoubleArrow SEDoubleArrow = True
  (==) SEEqual SEEqual = True
  (==) SEColon SEColon = True
  (==) SEComma SEComma = True
  (==) SEDollar SEDollar = True
  (==) SEIntLiteral SEIntLiteral = True
  (==) SEDoubleLiteral SEDoubleLiteral = True
  (==) SECharLiteral SECharLiteral = True
  (==) SEStringLiteral SEStringLiteral = True
  (==) _ _ = False

  -- normal implementation
Show SimpleExprTokenKind where
    show SESymbol = "SESymbol"
    show SEIgnore = "SEIgnore"
    show SELParen = "SELParen"
    show SERParen = "SERParen"
    show SELBracket = "SELBracket"
    show SERBracket = "SERBracket"
    show SEIdentifier = "SEIdentifier"
    show SEBackquote = "SEBackquote"
    show SEArrow =  "SEArrow"
    show SEDoubleArrow = "SEDoubleArrow"
    show SEEqual = "SEEqual"
    show SEColon = "SEColon"
    show SEComma = "SEComma"
    show SEDollar = "SEDollar"
    show SEIntLiteral = "SEIntLiteral"
    show SEDoubleLiteral = "SEDoubleLiteral"
    show SECharLiteral = "SECharLiteral"
    show SEStringLiteral = "SEStringLiteral"

 -- renaming Token type
public export
SimpleExprToken : Type 
SimpleExprToken = Token SimpleExprTokenKind

-- Kind and its content
export
Show SimpleExprToken where
 show (Tok kind text) = "Tok kind: " ++ show kind ++ " text: " ++ text

-- TokenType implementation contains defining their value type and value
-- symbol and identifier has their value as its own id.
-- nat has the corresponding Nat value.
export
TokenKind SimpleExprTokenKind where
  TokType SEIdentifier = String
  TokType SESymbol = String
  TokType SEIntLiteral = Integer
  TokType SEDoubleLiteral = Double
  TokType SECharLiteral = Char
  TokType SEStringLiteral = String
  TokType _ = ()

  tokValue SESymbol s = s
  tokValue SEIgnore _ = ()
  tokValue SELParen _ = ()
  tokValue SERParen _ = ()
  tokValue SELBracket _ = ()
  tokValue SERBracket _ = ()
  tokValue SEIdentifier s = s
  tokValue SEBackquote _ = ()
  tokValue SEArrow _ = ()
  tokValue SEDoubleArrow _ = ()
  tokValue SEEqual _ = ()
  tokValue SEColon _ = ()
  tokValue SEComma _ = ()
  tokValue SEDollar _ = ()
  tokValue SEIntLiteral s = fromMaybe 0 $ parseInteger s
  tokValue SEDoubleLiteral s = fromMaybe 0 $ parseDouble s
  tokValue SECharLiteral s =
    case unpack s of
      [] => '\0'
      (x :: []) => '\0'
      (x :: (y :: ys)) => y
  tokValue SEStringLiteral s = Data.String.strSubstr 1 (strLength s - 2) s -- Kind of bad since strSubstr is Int -> Int -> String -> String

--  ---lexer related functions---


-- same from Idris Source "Core.Name.isOpChar"
isOpChar : Char -> Bool
isOpChar c = c `elem` (unpack ":!#$%&*+./<=>?@\\^|-~")

-- same from Idris Source "Parser.Lexer.Source.validSymbol" 
symbolLexer : Lexer
symbolLexer = some (pred isOpChar)

reservedSyms : List (String, SimpleExprTokenKind)
reservedSyms = [
  ("->", SEArrow),
  ("=>", SEDoubleArrow),
  ("=", SEEqual),
  (":", SEColon),
  ("$", SEDollar)
]

-- same from Idris Source "Parser.Lexer.Source.doubleLit"
doubleLit : Lexer
doubleLit
    = digits <+> is '.' <+> digits <+> opt
           (is 'e' <+> opt (is '-' <|> is '+') <+> digits)

-- below are some of the strng literal lexing functions used in idris compiler
-- not used since they're complicated

-- same from Idris source "Parser.Lexer.Source."
-- stringBegin : Lexer
-- stringBegin = many (is '#') <+> (is '"')

-- same from Idris source "Parser.Lexer.Source.stringEnd"
-- stringEnd : Nat -> String
-- stringEnd hashtag = "\"" ++ replicate hashtag '#'

-- same from Idris source "Parser.Lexer.Source.multilineBegin"
-- multilineBegin : Lexer
-- multilineBegin = many (is '#') <+> (exact "\"\"\"") <+>
--                     manyUntil newline space <+> newline

-- same from Idris source "Parser.Lexer.Source.multilineEnd"
-- multilineEnd : Nat -> String
-- multilineEnd hashtag = "\"\"\"" ++ replicate hashtag '#'



-- from cookbook
-- need a support on underscore
nameLexer : Lexer
nameLexer =
    alpha <+> many (alphaNum <|> is '_' <|> is '\'')
  <|>
    is '(' <+> many spaces <+> symbolLexer <+> many spaces <+> is ')'

idLexer : Lexer
idLexer =
  many (pred isUpper <+> many alphaNum <+> is '.') <+> nameLexer


-- token map to tell what lexes to what
-- <SESymbol> ::= [:!#$%&*+./<=>\?@\\^|-~]+
-- <SELParen> ::= \(
-- <SERParen> ::= \)
-- <SEIdentifier> ::= [a-zA-Z][a-zA-Z0-9]*
-- <SEIgnore> ::= [空白文字]+
-- <SEBackquote> ::= `
-- <SEIntLiteral> ::= [0-9]+
-- <SEDoubleLiteral> ::= [0-9]+\\.[0-9]([e][+-]?[0-9]+)?
-- <SEStringLiteral> ::= "(\\.|.)"
simpleExprTokenMap : TokenMap SimpleExprToken
simpleExprTokenMap =
    toTokenMap [(spaces, SEIgnore)] ++
    toTokenMap [(idLexer, SEIdentifier )] ++
    [(symbolLexer, \s =>
      case lookup s reservedSyms of
        (Just kind) => Tok kind s
        Nothing => Tok SESymbol s
      )
    ] ++
    toTokenMap [
      (exact "(", SELParen),
      (exact ")", SERParen),
      (exact "{", SELBracket),
      (exact "}", SERBracket),
      (exact "`", SEBackquote),
      (exact ",", SEComma),
      (digits, SEIntLiteral),
      (doubleLit, SEDoubleLiteral),
      (charLit, SECharLiteral),
      (stringLit, SEStringLiteral)
    ]


-- the main lexer. uses the token map created above
export
lexSimpleExpr : String -> Maybe (List (WithBounds SimpleExprToken))
lexSimpleExpr str =
  case lex simpleExprTokenMap str of
    (tokens, _, _, "") => Just tokens
    _ => Nothing