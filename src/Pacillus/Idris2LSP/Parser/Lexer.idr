module Pacillus.Idris2LSP.Parser.Lexer

import Text.Lexer
import Data.List1
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
    | SEOperator
    | SEMember
    | SEMemberId
    | SEHole
    | SEBackquote
    | SEArrow
    | SEDoubleArrow
    | SEBackslash
    | SEEqual
    | SEColon
    | SEComma
    | SEDollar
    | SEDoubleStar
    | SEWildcard
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
  (==) SEOperator SEOperator = True
  (==) SEMember SEMember = True
  SEMemberId == SEMemberId = True
  SEHole == SEHole = True
  (==) SEBackquote SEBackquote = True
  (==) SEArrow SEArrow = True
  (==) SEDoubleArrow SEDoubleArrow = True
  SEBackslash == SEBackslash = True
  (==) SEEqual SEEqual = True
  (==) SEColon SEColon = True
  (==) SEComma SEComma = True
  (==) SEDollar SEDollar = True
  SEDoubleStar == SEDoubleStar = True
  (==) SEWildcard SEWildcard = True
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
    show SEOperator = "SEOperator"
    show SEMember = "SEMember"
    show SEMemberId = "SEMemberId"
    show SEHole = "SEHole"
    show SEBackquote = "SEBackquote"
    show SEArrow =  "SEArrow"
    show SEDoubleArrow = "SEDoubleArrow"
    show SEBackslash = "SEBackslash"
    show SEEqual = "SEEqual"
    show SEColon = "SEColon"
    show SEComma = "SEComma"
    show SEDollar = "SEDollar"
    show SEDoubleStar = "SEDoubleStar"
    show SEWildcard = "SEWildcard"
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

trimNamespace : String -> String
trimNamespace str = last $ split (\c => c == '.') str

disposeUntilLparen : String -> String
disposeUntilLparen str = last $ split (\c => c == '(') str

-- TokenType implementation contains defining their value type and value
-- symbol and identifier has their value as its own id.
-- ~Literal has the corresponding Literal Type value.
export
TokenKind SimpleExprTokenKind where
  TokType SEIdentifier = String
  TokType SEOperator = String
  TokType SEMember = String
  TokType SEMemberId = String
  TokType SEHole = String
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
  tokValue SEIdentifier s = trimNamespace s
  tokValue SEOperator s = 
    let s' = disposeUntilLparen s in
      trim $ strSubstr 0 ((cast $ length s') - 1) s'
  tokValue SEMember s =
      strSubstr 1 ((cast $ length s) - 1) s
  tokValue SEMemberId s =
    let s' = disposeUntilLparen s in
      trim $ strSubstr 1 ((cast $ length s') - 2) s'
  tokValue SEHole s =
    strSubstr 1 ((cast $ length s) - 1) s
  tokValue SEBackquote _ = ()
  tokValue SEArrow _ = ()
  tokValue SEDoubleArrow _ = ()
  tokValue SEBackslash _ = ()
  tokValue SEEqual _ = ()
  tokValue SEColon _ = ()
  tokValue SEComma _ = ()
  tokValue SEDollar _ = ()
  tokValue SEDoubleStar _ = ()
  tokValue SEWildcard _ = ()
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
  ("$", SEDollar),
  ("**", SEDoubleStar),
  ("\\", SEBackslash)
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
nameLexer : Lexer
nameLexer =
    alpha <+> many (alphaNum <|> is '_' <|> is '\'')

memberLexer : Lexer
memberLexer = is '.' <+> nameLexer

symbolIdLexer : Lexer
symbolIdLexer = many (pred isUpper <+> many alphaNum <+> is '.') <+> is '(' <+> many spaces <+> symbolLexer <+> many spaces <+> is ')'

memberIdLexer : Lexer
memberIdLexer = many (pred isUpper <+> many alphaNum <+> is '.') <+> is '(' <+> many spaces <+> memberLexer <+> many spaces <+> is ')'

idLexer : Lexer
idLexer =
  many (pred isUpper <+> many alphaNum <+> is '.') <+> nameLexer

holeLexer : Lexer
holeLexer =
  is '?' <+> nameLexer

-- token map to tell what lexes to what
-- <SESymbol> ::= [:!#$%&*+./<=>\?@\\^|-~]+
-- <SELParen> ::= \(
-- <SERParen> ::= \)
-- <SEIdentifier> ::= [a-zA-Z][a-zA-Z0-9]*
-- <SEIgnore> ::= [spaces]+
-- <SEBackquote> ::= `
-- <SEIntLiteral> ::= [0-9]+
-- <SEDoubleLiteral> ::= [0-9]+\\.[0-9]([e][+-]?[0-9]+)?
-- <SEStringLiteral> ::= "(\\.|.)"
simpleExprTokenMap : TokenMap SimpleExprToken
simpleExprTokenMap =
    toTokenMap [(spaces, SEIgnore)] ++
    toTokenMap [(idLexer, SEIdentifier )] ++
    toTokenMap [(memberLexer, SEMember)] ++
    toTokenMap [(holeLexer, SEHole)] ++
    toTokenMap [(symbolIdLexer, SEOperator)] ++
    toTokenMap [(memberIdLexer, SEMemberId)] ++
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
      (exact "_", SEWildcard),
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
