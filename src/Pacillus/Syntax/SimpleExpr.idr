module Pacillus.Syntax.SimpleExpr



import Data.List
import Data.List1
import Data.Maybe
import Data.String
import Text.Lexer
import Text.Parser
import Text.Parser.Expression

%default total

-- For people who want to make use of this code and have never used monad parser before,
-- You should try making one on your own. Haskell's monad parser is similar and will also be good for learning.
-- To do this, you should see the cookbook of lambda calculus parser shown in below.

-- this program was made referencing "Documentation for the Idris 2 Language/Cookbook/Parsing"
-- https://idris2.readthedocs.io/en/latest/cookbook/parsing.html


-- TODO Make Pair
-- TODO Add unit
-- TODO Add primitive type
-- TODO Add prefix to OpTable

-- ##Token##
-- <SESymbol> ::= [:!#$%&*+./<=>?@\^|-~]+
-- <SELParen> ::= '('
-- <SERParen> ::= ')'
-- <SEIdentifier> ::= [a-zA-Z][a-zA-Z0-9]+
-- <SEIgnore> ::= [空白文字]+
-- <SEBackquote> ::= [`]
-- <SEArrow> ::= [-][>]
-- <SEEqual> ::= [=]
-- <SEColon> ::= 
-- <SENatLiteral> ::= [0-9]+
-- <SEDoubleLiteral> ::= [0-9]+[.][0-9]([e][+-]?[0-9]+)?
-- <SEStringLiteral> ::= ["](\\.|.)["]

-- ##Syntax##
-- <typed> ::= <arrow>

-- <arrow> ::= 
--     <equal> <SEArrow> <arrow>
--   | <SELParen> <signature> <SERParen> <SEArrow> <arrow>
--   | <equal>

-- <signature> ::= <SEIdentifier> <SEColon> <arrow>

-- <equal> ::=
--    <expr> <SEEqual> <expr>
--  | <expr>

-- <expr> ::=
--     <expr> <infixOperator> <expr>
--   | <expr> <infixFunction> <expr>
--   | <lterm>

-- <lterm> ::=
--     <lterm> <term>
--   | <term>

-- <term> ::=
--     <var>
--   | <literal>
--   | <paren>

-- <var> ::= <SEIdentifier>

-- <literal> ::=
--     <SEIntLiteral>
--   | <SEDoubleLiteral>
--   | <SEStringLiteral>

-- <paren> ::= <SELParen> <typed> <SERParen> 

-- <infixOperator> ::= <SESymbol>

-- <infixFunction> ::= <SEBackquote> <SEIdentifier> <SEBackquote>

-- ## Abstract Syntax Tree ##
-- <SimpleExpr> ::= <Var> | <App> | <Equality> | <Arrow> | <IntegerLiteral> | <DoubleLiteral> | <StringLiteral> 
-- <Var> ::= String
-- <App> ::= <SimpleExpr> <SimpleExpr>
-- <Equality> ::= <SimpleExpr> <SimpleExpr>
-- <Arrow> ::= <SimpleExpr> <SijmpleExpr>
-- <IntegerLiteral> ::= Nat
-- <DoubleLiteral> ::= Double
-- <StringLiteral> ::= String

-- ---data type defentions---

-- the data type for AST of SimpleExpr
-- <SimpleExpr> ::= <Var> | <App> | <IntegerLiteral> | <DoubleLiteral> | <StringLiteral>
-- <Var> ::= String
-- <App> ::= <SimpleExpr> <SimpleExpr>
-- <IntegerLiteral> ::= Nat
-- <DoubleLiteral> ::= Double
-- <StringLiteral> ::= String
data SimpleExpr = Var String | App SimpleExpr SimpleExpr | Equality SimpleExpr SimpleExpr | Arrow SimpleExpr SimpleExpr | Signature String SimpleExpr | NatLiteral Nat | DoubleLiteral Double | StringLiteral String

-- information of operator used for parsing
data OpRecord = OpInfoTriple String Nat Assoc

InOperatorMap : Type
InOperatorMap = List OpRecord

sortOpMap : InOperatorMap -> InOperatorMap
sortOpMap opmap = (sortBy compRec opmap)
  where
    compRec : OpRecord -> OpRecord -> Ordering
    compRec (OpInfoTriple str n1 x) (OpInfoTriple str1 n2 y) = compare n1 n2



-- application is bracketed if needed
-- the argorithm is not thought through and probably needs improvement
Show SimpleExpr where
    showPrec d (Var str) = str
    showPrec d (App x y) = showParens (d == Prelude.App) (showPrec (User 0) x ++ " " ++ showPrec App y)
    showPrec d (Equality x y) = showPrec Equal x ++ "=" ++ showPrec Equal y
    showPrec d (Arrow x y) = showParens (d == (User 1)) (showPrec (User 1) x ++ "->" ++ showPrec (User 0) y)
    showPrec d (Signature var typeexpr) = var ++ ":" ++ show typeexpr
    showPrec d (NatLiteral n) = show n
    showPrec d (DoubleLiteral n) = show n
    showPrec d (StringLiteral s) = show s

-- data to identify tokens
data SimpleExprTokenKind =
      SESymbol
    | SEIgnore
    | SELParen
    | SERParen
    | SEIdentifier
    | SEBackquote
    | SEArrow
    | SEEqual
    | SEColon
    | SENatLiteral
    | SEDoubleLiteral
    | SEStringLiteral


-- normal implementation.
-- DO NOT FORGET TO ADD DEFENITON FOR NEW SYMBOL
-- IT IS SET TO FALSE BY DEFAULT AND IT IS EASY TO MISS
Eq SimpleExprTokenKind where
  (==) SESymbol SESymbol = True
  (==) SEIgnore SEIgnore = True
  (==) SELParen SELParen = True
  (==) SERParen SERParen = True
  (==) SEIdentifier SEIdentifier = True
  (==) SEBackquote SEBackquote = True
  (==) SEArrow SEArrow = True
  (==) SEEqual SEEqual = True
  (==) SEColon SEColon = True
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
  show SEBackquote = "SEBackquote"
  show SEArrow =  "SEArrow"
  show SEEqual = "SEEqual"
  show SEColon = "SEColon"
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
  tokValue SEBackquote _ = ()
  tokValue SEArrow _ = ()
  tokValue SEEqual _ = ()
  tokValue SEColon _ = ()
  tokValue SENatLiteral s = stringToNatOrZ s
  tokValue SEDoubleLiteral s = fromMaybe 0 $ parseDouble s
  tokValue SEStringLiteral s = strSubstr 1 (strLength s - 2) s -- Kind of bad since strSubstr is Int -> Int -> String -> String

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
  ("=", SEEqual),
  (":", SEColon)
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
idLexer : Lexer
idLexer =
  alpha <+> many alphaNum


-- token map to tell what lexes to what
-- <SESymbol> ::= [:!#$%&*+./<=>?@\^|-~]+
-- <SELParen> ::= '('
-- <SERParen> ::= ')'
-- <SEIdentifier> ::= [a-zA-Z][a-zA-Z0-9]+
-- <SEIgnore> ::= [空白文字]+
-- <SEBackquote> ::= [`]
-- <SENatLiteral> ::= [0-9]+
-- <SEDoubleLiteral> ::= [0-9]+[.][0-9]([e][+-]?[0-9]+)?
-- <SEStringLiteral> ::= ["](\\.|.)["]
simpleExprTokenMap : TokenMap SimpleExprToken
simpleExprTokenMap =
    toTokenMap [(spaces, SEIgnore)] ++
    toTokenMap [(idLexer, SEIdentifier )] ++
    --toTokenMap [(symbolLexer, SESymbol)] ++
    [(symbolLexer, \s =>
      case lookup s reservedSyms of
        (Just kind) => Tok kind s
        Nothing => Tok SESymbol s
      )
    ] ++
    toTokenMap [
      (exact "(", SELParen),
      (exact ")", SERParen),
      (exact "`", SEBackquote),
      (digits, SENatLiteral),
      (doubleLit, SEDoubleLiteral),
      (stringLit, SEStringLiteral)
    ]


-- the main lexer. uses the token map created above
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
simpleExprInf2App inid t1 t2 = App (App (Var  inid) t1) t2




-- the main parser
-- starts in expr
mutual
    -- <infixOperator> ::= <SESymbol>
    infixOperator : (symbol_name : String) -> Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr)
    infixOperator symbol_name =
      do
        sym <- match SESymbol
        when (sym /= symbol_name) $ fail "not a matching operator" -- only parses the symbol of arg
        pure $ simpleExprInf2App $ "(" ++ sym ++ ")"

    -- <singleOperator> ::= <SESymbol>
    singleOperator : Grammar state SimpleExprToken True SimpleExpr
    singleOperator =
      do
        sym <- match SESymbol
        pure $ Var $ "(" ++ sym ++ ")"

    -- <infixFunction> ::= <SEBackquote> <SEIdentifier> <SEBackquote>
    infixFunction : Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr)
    infixFunction =
      do
        match SEBackquote
        id <- match SEIdentifier
        match SEBackquote
        pure $ simpleExprInf2App id
    
    -- dynnamically constructs a OperatorTable for parsing expr
    dynOperatorTable : InOperatorMap -> OperatorTable state SimpleExprToken SimpleExpr
    dynOperatorTable opmap =
      let
        -- below is the data flow
        -- opmap -> norm_opmap -> mergerd_norm -> grouped -> sorted -> return!
        -- type for normalized Operator information. state is passed by arg
        OpNorm : Type -> Type
        OpNorm state = (Nat, Op state SimpleExprToken SimpleExpr)

        -- normalizes OpRecord
        norm_oprec : OpRecord -> OpNorm state
        norm_oprec (OpInfoTriple name prec assoc) = (prec, Infix (infixOperator name) assoc)

        -- converted to OpNorm
        norm_opmap : List (OpNorm state)
        norm_opmap = map norm_oprec opmap

        -- add infix functions
        -- proccess of adding prefix will later be added
        merged_norm : List (OpNorm state)
        merged_norm = (1, Infix infixFunction AssocNone) :: norm_opmap

        -- defines the equality of OpNorm, which is a equality of precedence
        -- then group them by the equality
        grouping : OpNorm state -> OpNorm state -> Bool
        grouping (prec1, _) (prec2, _) = prec1 == prec2
        grouped = groupBy grouping merged_norm

        -- defines the ordering of OpNorm, which is an ordering of OpNorm
        -- then order each group by the ordering
        -- arg of compare is opposite to make the ordering a descending order
        sorting : List1 (OpNorm state) -> List1 (OpNorm state) -> Ordering
        sorting ((prec1, _) ::: _) ((prec2, _) ::: _) = compare prec2 prec1
        sorted = sortBy sorting grouped

        -- forget any extra information, and converts type into final form
        -- List1 to List and OpNorm to Op
        forgetAll : List1 (OpNorm state) -> List (Op state SimpleExprToken SimpleExpr)
        forgetAll xs = map snd $ forget xs
      in
        map forgetAll sorted
    -- dynOperatorTable [] = [[], [Infix infixFunction AssocNone]]
    -- dynOperatorTable (x :: xs) = 
    --   let
    --     compiled = dynOperatorTable xs
    --     (OpInfoTriple symbol_name precedence assoc) = x
    --   in
    --     precedence (infixOperator symbol_name) compiled
    -- dynOperatorTable opmap = map (map) opmap
    -- dynOperatorTable map = [
    --           [Infix infixOperator AssocLeft],
    --           
    --         ]
    

    -- <typed> ::=
    --    <expr> <SEEqual> <expr>
    --  | <expr> <SEArrow> <typed>
    --  | <expr>
    typed : InOperatorMap -> Grammar state SimpleExprToken True SimpleExpr
    typed opmap =
      arrow opmap

    -- <arrow> ::= 
    --     <equal> <SEArrow> <arrow>
    --   | <SELParen> <signature> <SERParen> <SEArrow> <arrow>
    --   | <equal>
    arrow : InOperatorMap -> Grammar state SimpleExprToken True SimpleExpr
    arrow opmap =
      do
        ex1 <- equal opmap
        match SEArrow
        ex2 <- arrow opmap
        pure $ Arrow ex1 ex2
      <|>
      do
        match SELParen
        sig <- signature opmap
        match SERParen
        match SEArrow
        expr <- arrow opmap
        pure $ Arrow sig expr
      <|>
        equal opmap

    -- <signature> ::= <SEIdentifier> <SEColon> <arrow>
    signature : InOperatorMap -> Grammar state SimpleExprToken True SimpleExpr
    signature opmap =
      do
        v <- match SEIdentifier
        match SEColon
        expr <- arrow opmap
        pure $ Signature v expr

    -- "=" is a non associative operator
    -- <equal> ::=
    --    <expr> <SEEqual> <expr>
    --  | <expr>
    equal : InOperatorMap -> Grammar state SimpleExprToken True SimpleExpr
    equal opmap =
      do
        ex1 <- expr opmap
        match SEEqual
        ex2 <- expr opmap
        pure $ Equality ex1 ex2
      <|>
        expr opmap

    -- <expr> ::=
    --     <expr> <infixOperator> <expr>
    --   | <expr> <infixFunction> <expr>
    --   | <lterm>
    expr : InOperatorMap -> Grammar state SimpleExprToken True SimpleExpr
    expr opmap =
        buildExpressionParser (dynOperatorTable opmap) (lterm opmap)
      <|>
        singleOperator

    -- <lterm> ::=
    --     <lterm> <term>
    --   | <term>
    lterm : InOperatorMap -> Grammar state SimpleExprToken True SimpleExpr
    lterm opmap =
    do
      t <- term opmap
      app opmap t <|> pure t

    -- subfunction for lterm
    app : InOperatorMap -> SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    app opmap e1 = do
      e2 <- term opmap
      app1 opmap $ App e1 e2

    -- subfunction for lterm
    app1 : InOperatorMap -> SimpleExpr -> Grammar state SimpleExprToken False SimpleExpr
    app1 opmap e = app opmap e <|> pure e

    -- <term> ::=
    --     <var>
    --   | <literal>
    --   | <paren>
    term : InOperatorMap -> Grammar state SimpleExprToken True SimpleExpr
    term opmap =
        var <|> literal <|> paren opmap

    -- <var> ::= <SEIdentifier>
    var : Grammar state SimpleExprToken True SimpleExpr
    var = map Var $ match SEIdentifier

    -- <literal> ::=
    --     <SEIntLiteral>
    --   | <SEDoubleLiteral>
    --   | <SEStringLiteral>
    literal : Grammar state SimpleExprToken True SimpleExpr
    literal =
      do
        n <- match SENatLiteral
        pure $ NatLiteral n
      <|>
      do
        n <- match SEDoubleLiteral
        pure $ DoubleLiteral n
      <|>
      do
        s <- match SEStringLiteral
        pure $ StringLiteral s
      
    -- <paren> ::= <SELParen> <expr> <SERParen> 
    paren : InOperatorMap -> Grammar state SimpleExprToken True SimpleExpr
    paren opmap =
      do
        match SELParen
        e <-  arrow opmap
        match SERParen
        pure e

    

-- parses token list
parseSimpleExpr : List (WithBounds SimpleExprToken) -> Either String SimpleExpr
parseSimpleExpr toks =
  case parse (typed [OpInfoTriple "$" 0 AssocRight, OpInfoTriple "+" 8 AssocLeft, OpInfoTriple "*" 9 AssocLeft, (OpInfoTriple "===" 6 AssocNone)]) $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left (show xs)
    Left e => Left (show e)

-- parses string to AST
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


-- test5 doesn't work properly, don't know why
-- test1-test4 means the same as test5
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

-- ## string literal in Idris ##
str : String
str = "this is substr!"

texta : String
texta = ##"" \##{str}\#  #""# "#"##


