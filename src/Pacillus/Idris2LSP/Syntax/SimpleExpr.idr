module Pacillus.Idris2LSP.Syntax.SimpleExpr

import Data.List
import Data.List1
import Data.Maybe
import Text.Parser
import Text.Parser.Expression

import Pacillus.Idris2LSP.Syntax.Lexer

%default total

{-
For people who want to make use of this code and have never used monad parser before,
I recommend you to try making one on your own. Haskell's monad parser is similar and will also be good for learning.
For reference, cookbook of lambda calculus parser shown in below is a good example.

this program was made referencing "Documentation for the Idris 2 Language/Cookbook/Parsing"
https://idris2.readthedocs.io/en/latest/cookbook/parsing.html


TODO Add prefix to OpTable
TODO Arrow with bracketed Signature
TODO Partially filled infix notation

##Token##
<SESymbol> ::= [:!#$%&*+./<=>?@\^|-~]+
<SELParen> ::= '('
<SERParen> ::= ')'
<SEIdentifier> ::= [a-zA-Z][a-zA-Z0-9]+
<SEIgnore> ::= [空白文字]+
<SEBackquote> ::= [`]
<SEArrow> ::= [-][>]
<SEEqual> ::= [=]
<SEColon> ::= 
<SEIntLiteral> ::= [0-9]+
<SEDoubleLiteral> ::= [0-9]+[.][0-9]([e][+-]?[0-9]+)?
<SEStringLiteral> ::= ["](\\.|.)["]


##Syntax##
<simpleExpr> ::=
    <pair>
    <unit>
    <arrow>
  | <operation>

<arrow> ::= 
  | <operation> <SEArrow> <simpleExpr>
  | <SELParen> <signature> <SERParen> <SEArrow> <arrow>

<operation>
  | <operation> <infixOperator> <operation>
  | <operation> <infixFunction> <operation>
  | <operation> <SEEqual> <operation>
  | <app>
  | <term>

<signature> ::= <SEIdentifier> <SEColon> <expr>

<app> ::=
    <app> <term>
  | <identifier>
  | <appWithParen>

<appWithParen> ::= <SELParen> <app> <SERParen>

<term> ::=
    <identifier>
  | <literal>
  | <paren>

<identifier> ::= <SEIdentifier>

<literal> ::=
    <SEIntLiteral>
  | <SEDoubleLiteral>
  | <SEStringLiteral>

<paren> ::= <SELParen> <simpleExpr> <SERParen> 

<infixOperator> ::= <SESymbol>

<infixFunction> ::= <SEBackquote> <SEIdentifier> <SEBackquote>


## Abstract Syntax Tree ##
<SimpleExpr> ::=
   <IdTerm>
 | <AppTerm>
 | <EqTerm>
 | <ArwTerm>
 | <IntegerLiteral>
 | <DoubleLiteral>
 | <StringLiteral>
 | <PrTerm>
 | Unit
<AppTerm> ::= <Application>
<IntegerLiteral> ::= Integer
<DoubleLiteral> ::= Double
<StringLiteral> ::= String
<EqTerm> ::= <Equality>
<IdTerm> ::= <Identifier>
<ArwTerm> ::= <Arrow>
<PrTerm> ::= <Pair>

<Application> ::= <SimpleExpr> <SimpleExpr>

<Identifier> ::= String

<Equality> ::= <SimpleExpr> <SimpleExpr>

<Arrow> ::=
    <SimpleExpr> <SimpleExpr>
  | <Signature> <SimpleExpr>

<Pair> ::= <SimpleExpr> <SimpleExpr>

<Signature> ::= <Identifier> <SimpleExpr>
-}

-- ---data type defentions---


mutual
    -- the data type for AST of SimpleExpr
    -- <SimpleExpr> ::= <IdTerm> | <AppTerm> | <EqTerm> | <ArwTerm> | <IntegerLiteral> | <DoubleLiteral> | <StringLiteral> 
    -- <AppTerm> ::= <Application>
    -- <IntegerLiteral> ::= Integer
    -- <DoubleLiteral> ::= Double
    -- <StringLiteral> ::= String
    -- <EqTerm> ::= <Equality>
    -- <IdTerm> ::= <Identifier>
    -- <ArwTerm> ::= <Arrow>
    public export
    data SimpleExpr : Type where
        IdTerm : Identifier -> SimpleExpr
        AppTerm : Application -> SimpleExpr 
        ArwTerm : Arrow False -> SimpleExpr
        EqTerm : Equality -> SimpleExpr
        IntegerLiteral : Integer -> SimpleExpr
        DoubleLiteral : Double -> SimpleExpr
        StringLiteral : String -> SimpleExpr
        PrTerm : Pair -> SimpleExpr
        UnitTerm : SimpleExpr

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
    --     <SimpleExpr> <SimpleExpr>
    --   | <Signature> <SimpleExpr>
    public export
    data Arrow : (nosig : Bool) -> Type where
        ExExArr : {b : Bool} -> SimpleExpr -> SimpleExpr -> Arrow b
        SiExArr : Signature -> SimpleExpr -> Arrow False

    public export
    data Pair : Type where
        MkPair : SimpleExpr -> SimpleExpr -> Pair

    -- <Signature> ::= <SimpleExpr> <SimpleExpr>
    public export
    data Signature : Type where
      MkSignature : (name : Identifier) -> SimpleExpr -> Signature

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
    exEquality (PrTerm x) (PrTerm y) = prEquality x y
    exEquality (IntegerLiteral k1) (IntegerLiteral k2) = k1 == k2
    exEquality (DoubleLiteral dbl1) (DoubleLiteral dbl2) = dbl1 == dbl2
    exEquality (StringLiteral str1) (StringLiteral str2) = str1 == str2
    exEquality UnitTerm UnitTerm = True
    exEquality _ _ = False

    appEquality : Application -> Application -> Bool
    appEquality (MkApp x z) (MkApp y w) = exEquality x y && exEquality z w

    eqEquality : Equality -> Equality -> Bool
    eqEquality (MkEquality le1 re1) (MkEquality le2 re2) = exEquality le1 le2 && exEquality re1 re2

    prEquality : Pair -> Pair -> Bool
    prEquality (MkPair lfst lsnd) (MkPair rfst rsnd) = exEquality lfst rfst && exEquality lsnd rsnd

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

export
Show Identifier where
  show (MkId str) = str

mutual
  export
  showSimpleExpr : Nat -> SimpleExpr -> String
  showSimpleExpr d (IdTerm (MkId name)) = name
  showSimpleExpr d (AppTerm x) = showApp d x
  showSimpleExpr d (ArwTerm x) = showArw d x
  showSimpleExpr d (EqTerm x) = showEq d x
  showSimpleExpr d (PrTerm x) = showPr d x
  showSimpleExpr d (IntegerLiteral k) = show k
  showSimpleExpr d (DoubleLiteral dbl) = show dbl
  showSimpleExpr d (StringLiteral str) = show str
  showSimpleExpr d UnitTerm = "()"

  showApp : Nat -> Application -> String
  showApp d (MkApp x y) = showParens (d >= 3) $ showSimpleExpr 2 x ++ " " ++ showSimpleExpr 3 y

  showArw : Nat -> Arrow False -> String
  showArw d (ExExArr x y) = showParens (d >= 2) $ showSimpleExpr 2 x ++ " -> " ++ showSimpleExpr 1 y
  showArw d (SiExArr (MkSignature name x) y) = showParens (d >= 2) $ "(" ++ show name ++ " : " ++ showSimpleExpr 0 x ++ ") -> " ++ showSimpleExpr 2 y

  showEq : Nat -> Equality -> String
  showEq d (MkEquality x y) = showParens (d >= 1) $ showSimpleExpr 1 x ++ "=" ++ showSimpleExpr 1 y

  showPr : Nat -> Pair -> String
  showPr d x = "(" ++ showPrNoBracket x ++ ")"

  showPrNoBracket : Pair -> String
  showPrNoBracket (MkPair x (PrTerm y)) = showSimpleExpr 0 x ++ ", " ++ showPrNoBracket y
  showPrNoBracket (MkPair x y) = showSimpleExpr 0 x ++ ", " ++ showSimpleExpr 0 y

export
Show SimpleExpr where
  show x = showSimpleExpr 0 x

export
Show Signature where
  show (MkSignature name x) = show name ++ " : " ++ showSimpleExpr 0 x

-- information of operator used for parsing
data OpRecord = MkOpRecord String Nat Assoc

InOperatorMap : Type
InOperatorMap = List OpRecord

sortOpMap : InOperatorMap -> InOperatorMap
sortOpMap opmap = (sortBy compRec opmap)
  where
    compRec : OpRecord -> OpRecord -> Ordering
    compRec (MkOpRecord str n1 x) (MkOpRecord str1 n2 y) = compare n1 n2



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
        map ArwTerm $ arrow optable
      <|>
        operation optable

    pair : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Pair
    pair optable = 
      do
        match SELParen
        p <- pairSub optable
        match SERParen
        pure p

    pairSub : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Pair
    pairSub optable =
      do
        e <- simpleExpr optable
        match SEComma
        p <- pairSub optable
        pure $ (MkPair e $ PrTerm p)
      <|>
      do
        e1 <- simpleExpr optable
        match SEComma
        e2 <- simpleExpr optable
        pure $ (MkPair e1 e2)

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
    
    -- specially parsed using optable
    -- includes infix function, infix operation, and equality
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

    -- <signature> ::= <SEIdentifier> <SEColon> <SimpleExpr>
    export
    signature : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Signature
    signature optable = 
      do
        id <- match SEIdentifier
        match SEColon
        e <- simpleExpr optable
        pure $ MkSignature (MkId id) e



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

    -- <appWithParen> ::= <SELParen> <app> <SERParen>
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
    --     <unit>
    --     <pair>
    --     <var>
    --   | <literal>
    --   | <paren>
    term : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    term optable =
      do
        match SELParen
        match SERParen
        pure UnitTerm
      <|>
      do
        map PrTerm $ pair optable
      <|>
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
        pure $ IntegerLiteral n
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
        MkOpRecord "++" 7 AssocRight,
        MkOpRecord ">=" 6 AssocNone,
        MkOpRecord "::" 7 AssocRight,
        MkOpRecord "." 9 AssocRight
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





