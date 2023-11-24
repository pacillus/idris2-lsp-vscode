module Pacillus.Idris2LSP.Syntax.SimpleExpr

import Data.List
import Data.List1
import Data.Maybe
import Text.Parser
import Text.Parser.Expression

import Pacillus.Idris2LSP.Syntax.Lexer

%default total

-- For people who want to make use of this code and have never used monad parser before,
-- I recommend you to try making one on your own. Haskell's monad parser is similar and will also be good for learning.
-- For reference, cookbook of lambda calculus parser shown in below is a good example.

-- this program was made referencing "Documentation for the Idris 2 Language/Cookbook/Parsing"
-- https://idris2.readthedocs.io/en/latest/cookbook/parsing.html


-- TODO Make Pair
-- TODO Add unit
-- TODO Add prefix to OpTable
-- TODO Arrow with bracketed Signature

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
-- <SEIntLiteral> ::= [0-9]+
-- <SEDoubleLiteral> ::= [0-9]+[.][0-9]([e][+-]?[0-9]+)?
-- <SEStringLiteral> ::= ["](\\.|.)["]

-- ##Syntax##
-- <simpleExpr> ::=
--     <arrow>
--   | <operation>

-- <arrow> ::= 
--   | <operation> <SEArrow> <simpleExpr>
--   | <SELParen> <signature> <SERParen> <SEArrow> <arrow>

-- <operation>
--   | <operation> <infixOperator> <operation>
--   | <operation> <infixFunction> <operation>
--   | <operation> <SEEqual> <operation>
--   | <app>
--   | <term>

-- <signature> ::= <SEIdentifier> <SEColon> <expr>

-- <app> ::=
--     <app> <term>
--   | <identifier>
--   | <appWithParen>



-- <appWithParen> ::=
--     <SELParen> <app> <SERParen>


-- <term> ::=
--     <identifier>
--   | <literal>
--   | <paren>

-- <identifier> ::= <SEIdentifier>

-- <literal> ::=
--     <SEIntLiteral>
--   | <SEDoubleLiteral>
--   | <SEStringLiteral>


-- <paren> ::= <SELParen> <simpleExpr> <SERParen> 

-- <infixOperator> ::= <SESymbol>

-- <infixFunction> ::= <SEBackquote> <SEIdentifier> <SEBackquote>

-- ## Abstract Syntax Tree ##
-- <SimpleExpr> ::= <IdTerm> | <AppTerm> | <EqTerm> | <ArwTerm> | <IntegerLiteral> | <DoubleLiteral> | <StringLiteral> 
-- <AppTerm> ::= <Application>
-- <IntegerLiteral> ::= Integer
-- <DoubleLiteral> ::= Double
-- <StringLiteral> ::= String
-- <EqTerm> ::= <Equality>
-- <IdTerm> ::= <Identifier>
-- <ArwTerm> ::= <Arrow>

-- <Application> ::= <SimpleExpr> <SimpleExpr>

-- <Identifier> ::= String

-- <Equality> ::= <SimpleExpr> <SimpleExpr>

-- <Arrow> ::=
--     <SimpleExpr> <SimpleExpr>
--   | <Signature> <SimpleExpr>

-- <Signature> ::= String <Expr>

-- ---data type defentions---

-- the data type for AST of SimpleExpr
-- <SimpleExpr> ::= <IdTerm> | <AppTerm> | <EqTerm> | <ArwTerm> | <IntegerLiteral> | <DoubleLiteral> | <StringLiteral> 
-- <AppTerm> ::= <Application>
-- <IntegerLiteral> ::= Integer
-- <DoubleLiteral> ::= Double
-- <StringLiteral> ::= String
-- <EqTerm> ::= <Equality>
-- <IdTerm> ::= <Identifier>
-- <ArwTerm> ::= <Arrow>
mutual
    public export
    data SimpleExpr : Type where
        IdTerm : Identifier -> SimpleExpr
        AppTerm : Application -> SimpleExpr 
        ArwTerm : Arrow False -> SimpleExpr
        EqTerm : Equality -> SimpleExpr
        NatLiteral : Nat -> SimpleExpr
        DoubleLiteral : Double -> SimpleExpr
        StringLiteral : String -> SimpleExpr

    -- <Identifier> ::= String
    public export
    data Identifier : Type where
        MkId : String -> Identifier

    -- <Application> ::= <SimpleExpr> <SimpleExpr>
    public export
    data Application : Type where
        MkApp : SimpleExpr -> SimpleExpr -> Application

    -- <Equality> ::= <Expr> <Expr>
    public export
    data Equality = MkEquality SimpleExpr SimpleExpr

    -- Arrow True eliminates the pattern of SiExArr
    -- <Arrow> ::=
    --     <Expr> <Expr>
    --   | <Signature> <Expr>
    public export
    data Arrow : (nosig : Bool) -> Type where
        ExExArr : {b : Bool} -> SimpleExpr -> SimpleExpr -> Arrow b
        SiExArr : Signature -> SimpleExpr -> Arrow False

    -- <Signature> ::= String <Expr>
    public export
    data Signature = MkSignature String SimpleExpr

-- Arrow 
export
forgetSig : Arrow b -> Arrow True
forgetSig (ExExArr x y) = ExExArr x y
forgetSig (SiExArr (MkSignature str x) y) = ExExArr x y

export
Eq Identifier where
    (==) (MkId str) (MkId str1) = str == str1

mutual
    export
    exEquality : SimpleExpr -> SimpleExpr -> Bool
    exEquality (IdTerm x) (IdTerm y) = x == y
    exEquality (AppTerm x) (AppTerm y) = appEquality x y
    exEquality (ArwTerm x) (ArwTerm y) = sameTypeArw x y
    exEquality (EqTerm x) (EqTerm y) = eqEquality x y
    exEquality (NatLiteral k1) (NatLiteral k2) = k1 == k2
    exEquality (DoubleLiteral dbl1) (DoubleLiteral dbl2) = dbl1 == dbl2
    exEquality (StringLiteral str1) (StringLiteral str2) = str1 == str2
    exEquality _ _ = False

    appEquality : Application -> Application -> Bool
    appEquality (MkApp x z) (MkApp y w) = exEquality x y && exEquality z w

    eqEquality : Equality -> Equality -> Bool
    eqEquality (MkEquality le1 re1) (MkEquality le2 re2) = exEquality le1 le2 && exEquality re1 re2


    sameTypeArw : Arrow b1 -> Arrow b2 -> Bool
    sameTypeArw (ExExArr x z) (ExExArr y w) = exEquality x z && exEquality y w
    sameTypeArw (ExExArr x z) (SiExArr y w) =
        let MkSignature _ yex = y in exEquality x z && exEquality yex w
    sameTypeArw (SiExArr x z) (ExExArr y w) =
        let MkSignature _ xex = x in exEquality xex z && exEquality y w
    sameTypeArw (SiExArr x z) (SiExArr y w) =
        let 
          MkSignature _ xex = x
          MkSignature _ yex = y
        in exEquality xex z && exEquality yex w

mutual
  export
  showSimpleExpr : Nat -> SimpleExpr -> String
  showSimpleExpr d (IdTerm (MkId name)) = name
  showSimpleExpr d (AppTerm x) = showApp d x
  showSimpleExpr d (ArwTerm x) = showArw d x
  showSimpleExpr d (EqTerm x) = showEq d x
  showSimpleExpr d (NatLiteral k) = show k
  showSimpleExpr d (DoubleLiteral dbl) = show dbl
  showSimpleExpr d (StringLiteral str) = str

  showApp : Nat -> Application -> String
  showApp d (MkApp x y) = showParens (d >= 3) $ showSimpleExpr 2 x ++ " " ++ showSimpleExpr 3 y

  showArw : Nat -> Arrow False -> String
  showArw d (ExExArr x y) = showParens (d >= 2) $ showSimpleExpr 2 x ++ " -> " ++ showSimpleExpr 1 y
  showArw d (SiExArr (MkSignature str x) y) = showParens (d >= 2) $ "(" ++ str ++ " : " ++ showSimpleExpr 0 x ++ ") -> " ++ showSimpleExpr 2 y

  showEq : Nat -> Equality -> String
  showEq d (MkEquality x y) = showParens (d >= 1) $ showSimpleExpr 1 x ++ "=" ++ showSimpleExpr 1 y

export
Show Signature where
  show (MkSignature str x) = str ++ " : " ++ showSimpleExpr 0 x

-- information of operator used for parsing
data OpRecord = MkOpRecord String Nat Assoc

InOperatorMap : Type
InOperatorMap = List OpRecord

sortOpMap : InOperatorMap -> InOperatorMap
sortOpMap opmap = (sortBy compRec opmap)
  where
    compRec : OpRecord -> OpRecord -> Ordering
    compRec (MkOpRecord str n1 x) (MkOpRecord str1 n2 y) = compare n1 n2



-- application is bracketed if needed
-- the argorithm is not thought through and probably needs improvement
-- Show SimpleExpr where
--     showPrec d ( str) = str
--     showPrec d (App x y) = showParens (d == Prelude.App) (showPrec (User 0) x ++ " " ++ showPrec App y)
--     -- showPrec d (Equality x y) = showPrec Equal x ++ "=" ++ showPrec Equal y
--     -- showPrec d (Arrow x y) = showParens (d == (User 1)) (showPrec (User 1) x ++ "->" ++ showPrec (User 0) y)
--     -- showPrec d (Signature var typeexpr) = var ++ ":" ++ show typeexpr
--     showPrec d (NatLiteral n) = show n
--     showPrec d (DoubleLiteral n) = show n
--     showPrec d (StringLiteral s) = show s



-- ---Parser related functions---
-- defining what Tokens to ignore
export
ignored : WithBounds SimpleExprToken -> Bool
ignored (MkBounded (Tok SEIgnore _) _ _) = True
ignored _ = False

-- converting infix into application form
-- used in main parser
simpleExprInf2App : String -> SimpleExpr -> SimpleExpr -> SimpleExpr
simpleExprInf2App inid t1 t2 =
  let
    infixid = (IdTerm (MkId inid))
    firstapp = AppTerm $ MkApp infixid t1
  in
    AppTerm $ MkApp firstapp t2


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
    pure $ IdTerm $ MkId $ "(" ++ sym ++ ")"

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
    norm_oprec (MkOpRecord name prec assoc) = (prec, Infix (infixOperator name) assoc)

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

-- this is parsed using optable
equality : Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr)
equality =
  do
    match SEEqual
    pure $ \x,y => EqTerm (MkEquality x y)

-- the main parser
-- starts in expr
mutual
    -- <simpleExpr> ::=
    --     <arrow>
    --   | <operation>
    simpleExpr : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    simpleExpr optable =
      do
        e <- arrow optable
        pure $ ArwTerm e
      <|>
        operation optable

    -- <arrow> ::= 
    --   | <operation> <SEArrow> <expr>
    --   | <SELParen> <signature> <SERParen> <SEArrow> <arrow>
    arrow : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True (Arrow False)
    arrow optable =
      do
        e1 <- operation optable
        match SEArrow
        e2 <- simpleExpr optable
        pure $ ExExArr e1 e2
      <|>
      do
        match SELParen
        sig <- signature optable
        match SERParen
        match SEArrow
        e <- simpleExpr optable
        pure $ SiExArr sig e
    
    -- <operation>
    --   | <operation> <infixOperator> <operation>
    --   | <operation> <infixFunction> <operation>
    --   | <operation> <SEEqual> <operation>
    --   | <app>
    --   | <term>
    operation : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    operation optable =
        buildExpressionParser (optable ++ [[Infix equality AssocNone]]) (map AppTerm (app optable) <|> term optable)
      <|>
        term optable
      <|>
        singleOperator

    -- <signature> ::= <SEIdentifier> <SEColon> <expr>
    export
    signature : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Signature
    signature optable = 
      do
        id <- match SEIdentifier
        match SEColon
        e <- simpleExpr optable
        pure $ MkSignature id e



    -- left most part of application must be a identifier
    -- <app> ::=
    --     <app> <term>
    --   | <identifier> <term>
    --   | <appWithParen>
    app : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Application
    app optable =
      do
        id <- identifier
        t <- term optable
        appSub1 optable $ MkApp (IdTerm id) t
      <|>
      do
        a <- appWithParen optable
        t <- term optable
        appSub1 optable a

    -- <appWithParen> ::=
    --     <SELParen> <app> <SERParen>
    appWithParen :  OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Application
    appWithParen optable =
      do
        match SELParen
        a <- app optable
        match SERParen
        pure $ a

    -- subfunction for app
    appSub1 : OperatorTable state SimpleExprToken SimpleExpr -> Application -> Grammar state SimpleExprToken False Application
    appSub1 optable e = appSub2 optable e <|> pure e

    -- subfunction for app
    appSub2 : OperatorTable state SimpleExprToken SimpleExpr -> Application -> Grammar state SimpleExprToken True Application
    appSub2 optable app = do
      t <- term optable
      appSub1 optable $ MkApp (AppTerm app) t

    -- <term> ::=
    --     <var>
    --   | <literal>
    --   | <paren>
    term : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    term optable =
      do
        id <- identifier 
        pure $ IdTerm id
      <|> literal <|> paren optable

    -- <var> ::= <SEIdentifier>
    identifier : Grammar state SimpleExprToken True Identifier
    identifier =
        map MkId (match SEIdentifier)
      <|>
      do
        match SELParen
        id <- identifier
        match SERParen
        pure id

    -- <literal> ::=
    --     <SEIntLiteral>
    --   | <SEDoubleLiteral>
    --   | <SEStringLiteral>
    literal : Grammar state SimpleExprToken True SimpleExpr
    literal =
      do
        n <- match SEIntLiteral
        pure $ NatLiteral n
      <|>
      do
        n <- match SEDoubleLiteral
        pure $ DoubleLiteral n
      <|>
      do
        s <- match SEStringLiteral
        pure $ StringLiteral s
      
    -- <paren> ::= <SELParen> <simplExpr> <SERParen> 
    paren : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    paren optable =
      do
        match SELParen
        e <-  simpleExpr optable
        match SERParen
        pure e
    

    

opMap : InOperatorMap
opMap = 
    [
        MkOpRecord "$" 0 AssocRight,
        MkOpRecord "+" 8 AssocLeft, 
        MkOpRecord "*" 9 AssocLeft, 
        MkOpRecord "===" 6 AssocNone,
        MkOpRecord "++" 7 AssocRight
    ]

export
opTable : OperatorTable state SimpleExprToken SimpleExpr
opTable = dynOperatorTable opMap

-- parses token list
parseSimpleExpr : List (WithBounds SimpleExprToken) -> Either String SimpleExpr
parseSimpleExpr toks =
  case parse (simpleExpr opTable) $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left $ show xs -- Left "contains tokens that were not consumed"
    Left e => Left (show e)

-- parses string to AST
export
parse : String -> Either String SimpleExpr
parse x =
  case lexSimpleExpr x of
    Just toks => parseSimpleExpr toks
    Nothing => Left "Failed to lex."


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


