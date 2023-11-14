module Pacillus.Syntax



import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Text.Lexer
import Text.Parser
import Text.Parser.Expression

%default total

-- For people who want to make use of this code and have never used monad parser before,
-- You should try making one one your own. Haskell's monad parser is similar and will also be good for learning.
-- To do this, you should see the cookbook of lambda calculus parser shown in below.

-- this program was made referencing "Documentation for the Idris 2 Language/Cookbook/Parsing"
-- https://idris2.readthedocs.io/en/latest/cookbook/parsing.html

-- <Infix> ::= 
--     <Symbol>
--   | '`' [名前に使用可能な文字]+ '`'
-- <Name> ::= 
--     [名前に使用可能な文字]+
--   | '(' <Symbol> ')'
-- <Symbol> ::= [:!#$%&*+./<=>?@\^|-~]+
-- <LParen> ::= '('
-- <RParen> ::= ')'

-- # リテラルを追加するなら次も加える
-- <TextLiteral> ::= '"' 任意の文字列 '"'
-- <NaturalLiteral> ::= ['1'-'9']+['0'-'9']* | '0'

-- #構文
-- <AtomicSimpleExpr> ::=
--     <Identifier>
--   | <Literal>
--   | <LParen> <SimpleExpr> <RParen>

-- <Apply> ::=
--   | <SimpleExpr> <AtomicSimpleExpr>


-- <SimpleExpr> ::=
--   | <AtomicSimpleExpr>
--   | <SimpleExpr> <AtomicSimpleExpr>
--   | <SimpleExpr> <Infix> <SimpleExpr>

-- <expr> ::= 

-- 抽象構文
-- <SimpleExpr> ::= <Name> | <SimpleExpr> <SimpleExpr>

-- ---data type defentions---

-- the data type for AST of SimpleExpr
data SimpleExpr = Term String | App SimpleExpr SimpleExpr | NatLiteral Nat | DoubleLiteral Double

-- application is bracketed if needed
Show SimpleExpr where
    showPrec d (Term str) = str
    showPrec d (App x y) = showParens (d == Prelude.App) (showPrec (User 0) x ++ " " ++ showPrec App y)
    showPrec d (NatLiteral n) = show n
    showPrec d (DoubleLiteral n) = show n

-- data to identify tokens
data SimpleExprTokenKind =
      SESymbol
    | SEIgnore
    | SELParen
    | SERParen
    | SEIdentifier
    | SEBackQuote
    | SENatLiteral
    | SEDoubleLiteral
    | SEStringLiteral


-- normal implementation.I want them by default.
-- DO NOT FORGET TO ADD DEFENITON FOR NEW SYMBOL
-- IT IS SET TO FALSE BY DEFAULT AND IT IS EASY TO MISS
Eq SimpleExprTokenKind where
  (==) SESymbol SESymbol = True
  (==) SEIgnore SEIgnore = True
  (==) SELParen SELParen = True
  (==) SERParen SERParen = True
  (==) SEIdentifier SEIdentifier = True
  (==) SEBackQuote SEBackQuote = True
  (==) SENatLiteral SENatLiteral = True
  (==) SEDoubleLiteral SEDoubleLiteral = True
  (==) SEStringLiteral SEStringLiteral = True
  (==) _ _ = False

-- renaming Token type
SimpleExprToken : Type 
SimpleExprToken = Token SimpleExprTokenKind

-- normal implementation
Show SimpleExprTokenKind where
  show SESymbol = "SESymbol"
  show SEIgnore = "SEIgnore"
  show SELParen = "SELParen"
  show SERParen = "SERParen"
  show SEIdentifier = "SEIdentifier"
  show SEBackQuote = "SEBackQuote"
  show SENatLiteral = "SENatLiteral"
  show SEDoubleLiteral = "SEDoubleLiteral"
  show SEStringLiteral = "SEStringLiteral"

-- Kind and its content
Show SimpleExprToken where
 show (Tok kind text) = "Tok kind: " ++ show kind ++ " text: " ++ text

-- TokenType implementation contains defining their value type and value
-- symbol and identifier has their value as its own id.
-- nat has the corresponding Nat value.
TokenKind SimpleExprTokenKind where
  TokType SEIdentifier = String
  TokType SESymbol = String
  TokType SENatLiteral = Nat
  TokType SEDoubleLiteral = Double
  TokType SEStringLiteral = String
  TokType _ = ()

  tokValue SESymbol s = s
  tokValue SEIgnore _ = ()
  tokValue SELParen _ = ()
  tokValue SERParen _ = ()
  tokValue SEIdentifier s = s
  tokValue SEBackQuote _ = ()
  tokValue SENatLiteral s = stringToNatOrZ s
  tokValue SEDoubleLiteral s = fromMaybe 0 $ parseDouble s
  tokValue SEStringLiteral s = s

--  ---lexer related functions---


-- same from Idris Source "Core.Name.isOpChar"
isOpChar : Char -> Bool
isOpChar c = c `elem` (unpack ":!#$%&*+./<=>?@\\^|-~")

-- same from Idris Source "Parser.Lexer.Source.validSymbol" 
symbolLexer : Lexer
symbolLexer = some (pred isOpChar)

-- same from Idris Source "Parser.Lexer.Source.doubleLit"
doubleLit : Lexer
doubleLit
    = digits <+> is '.' <+> digits <+> opt
           (is 'e' <+> opt (is '-' <|> is '+') <+> digits)

-- stringLit : Lexer
-- stringLit = is '"' <+> many any <+> is '"'
-- same from Idris source "Parser.Lexer.Source."
stringBegin : Lexer
stringBegin = many (is '#') <+> (is '"')

-- same from Idris source "Parser.Lexer.Source.stringEnd"
stringEnd : Nat -> String
stringEnd hashtag = "\"" ++ replicate hashtag '#'

-- same from Idris source "Parser.Lexer.Source.multilineBegin"
multilineBegin : Lexer
multilineBegin = many (is '#') <+> (exact "\"\"\"") <+>
                    manyUntil newline space <+> newline

-- same from Idris source "Parser.Lexer.Source.multilineEnd"
multilineEnd : Nat -> String
multilineEnd hashtag = "\"\"\"" ++ replicate hashtag '#'


-- from cookbook
-- need a support on underscore
idLexer : Lexer
idLexer = alpha <+> many alphaNum

-- token map to tell what lexes to what
simpleExprTokenMap : TokenMap SimpleExprToken
simpleExprTokenMap = toTokenMap [(spaces, SEIgnore)] ++
    toTokenMap [(idLexer, SEIdentifier )] ++
    toTokenMap [(symbolLexer, SESymbol)] ++
    toTokenMap [
    (exact "(", SELParen),
    (exact ")", SERParen)
    ] ++
    toTokenMap [(exact "`", SEBackQuote)] ++
    toTokenMap [(digits, SENatLiteral)] ++
    toTokenMap [(doubleLit, SEDoubleLiteral)]

-- the main lexer. uses token map
lexSimpleExpr : String -> Maybe (List (WithBounds SimpleExprToken))
lexSimpleExpr str =
  case lex simpleExprTokenMap str of
    (tokens, _, _, "") => Just tokens
    _ => Nothing


-- ---Parser related functions---
-- defining what Tokens to ignore
ignored : WithBounds SimpleExprToken -> Bool
ignored (MkBounded (Tok SEIgnore _) _ _) = True
ignored _ = False

-- converting infix into application form
-- used in main parser
simpleExprInf2App : String -> SimpleExpr -> SimpleExpr -> SimpleExpr
simpleExprInf2App inid t1 t2 = App (App (Term  inid) t1) t2

-- the main parser
-- starts in expr
mutual
    expr : Grammar state SimpleExprToken True SimpleExpr
    expr =
        buildExpressionParser 
        [
          [Infix infixOperator AssocLeft],
          [Infix infixFunction AssocLeft]
        ] lterm
      <|>
        lterm

    lterm : Grammar state SimpleExprToken True SimpleExpr
    lterm =
    do
      t <- term
      app t <|> pure t

    term : Grammar state SimpleExprToken True SimpleExpr
    term =
        var <|> literal <|> paren 
      

    app : SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    app e1 = do
      e2 <- term
      app1 $ App e1 e2

    app1 : SimpleExpr -> Grammar state SimpleExprToken False SimpleExpr
    app1 e = app e <|> pure e

    var : Grammar state SimpleExprToken True SimpleExpr
    var = map Term $ match SEIdentifier

    literal : Grammar state SimpleExprToken True SimpleExpr
    literal =
      do
        n <- match SENatLiteral
        pure $ NatLiteral n
      <|>
      do
        n <- match SEDoubleLiteral
        pure $ DoubleLiteral n
      

    paren : Grammar state SimpleExprToken True SimpleExpr
    paren =
      do
        match SELParen
        e <-  expr
        match SERParen
        pure e

    infixOperator : Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr)
    infixOperator =
      do
        sym <- match SESymbol
        pure $ simpleExprInf2App $ "(" ++ sym ++ ")"

    infixFunction : Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr)
    infixFunction =
      do
        match SEBackQuote
        id <- match SEIdentifier
        match SEBackQuote
        pure $ simpleExprInf2App id
      
parseSimpleExpr : List (WithBounds SimpleExprToken) -> Either String SimpleExpr
parseSimpleExpr toks =
  case parse expr $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left (show xs)
    Left e => Left (show e)

parse : String -> Either String SimpleExpr
parse x =
  case lexSimpleExpr x of
    Just toks => parseSimpleExpr toks
    Nothing => Left "Failed to lex."

-- main : IO ()
-- main = 
--   let
--     caseList = 
--     [ "x", 
--       "x+x" ]
--     parsedListM = map lexSimpleExpr caseList
--     -- parsedList = map (fromMaybe []) parsedListM
--     test1 = map (fromMaybe []) [Just [], Just []]
--     test2 = ["hoge", "fuga"]
--     test3 = map (\x => Just [Prelude.String.(++) x "!"]) test2
--     test4 = map (fromMaybe []) test3
--     test5 = map (\x => foldl Prelude.String.(++) "" x) test4
--   in
--     putStrLn ?rhs


test1 : List String
test1 = ["hoge", "fuga"]

test2 : List (Maybe (List String))
test2 = map (\x => Just [Prelude.String.(++) x "!"]) test1

test3 : List (List String)
test3 = map (fromMaybe []) test2

test4 : List String
test4 = map (foldl Prelude.String.(++) "") test3

-- test5 : List String
-- test5 =
--   let 
--     sub1 = ["hoge", "fuga"]
--     sub2 = map (\x => Just [Prelude.String.(++) x "!"]) sub1
--     sub3 = map (fromMaybe []) sub2
--     sub4 = map (foldl Prelude.String.(++) "") sub3
--   in
--     sub4

texta : String
texta = ##"" \\#{test}\#  #""# "#"##


