module Pacillus.Idris2LSP.Syntax.SimpleExpr

import Data.List
import Data.List1
import Data.Maybe
import Data.String
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

** SimpleExpr is NOT the SimpleExpr in Idris syntax so be careful! **

TODO Auto Argument "type =>" (almost done
TODO Operators with . will not work well(display problem)
TODO Implicit arguments "{a : Type} ->" (almost done
TODO Alpha equivalence (i.e. parse "(x : Nat) -> f x" == parse "(y : Nat) -> f y" = True
TODO Add prefix to OpTable
TODO Partially filled infix notation "(+3)"
TODO lambda expression "\x=>x"
TODO quantitive types "(0 x : a) ->"
TODO Dependent pair sugar syntax "(x : a ** f x)"
TODO List sugar syntax  "[1, 2, 3]" for "1 :: 2 :: 3 :: Nil", "[]" for "Nil"
TODO explicit argument "@{x}"
TODO infer type of partial expression with holes
TODO right now (a, b) is interpreted as "Pair a b" when it may also be "MkPair a b"
     fix this
TODO () is a sugar syntax for Unit when type and MkUnit when term context
TODO Unification between renamed type and not renamed 
TODO Namespace A.B.f
TODO projection sugar syntax "dat .member"
TODO normalize expression on unification to absorb differnces between any two expression that relates by beta equivalence

List of things not gonna do
・let, case, do, if, and user defined syntax

##Token##
<SESymbol> ::= [:!#$%&*+./<=>?@\\^|-~]+
<SELParen> ::= (
<SERParen> ::= )
<SEIdentifier> ::= [a-zA-Z][a-zA-Z0-9]+
<SEIgnore> ::= [空白文字]+
<SEBackquote> ::= `
<SEArrow> ::= ->
<SEEqual> ::= =
<SEColon> ::= :
<SEIntLiteral> ::= [0-9]+
<SEDoubleLiteral> ::= [0-9]+\.[0-9](e[\+\-]?[0-9]+)?
<SEStringLiteral> ::= "(\\.|.)"


##Syntax##
<simpleExpr> ::=
    <pair>
    <unit>
    <arrow>
  | <operation>

<arrow> ::= 
  | <operation> <SEArrow> <simpleExpr>
  | <SELParen> <signature> <SERParen> <SEArrow> <simplExpr>
  | <SELBracket> <signature> <SERParen> <SEArrow> <simpleExpr>

<operation>
  | <operation> <infixOperator> <operation>
  | <operation> <infixFunction> <operation>
  | <operation> <SEEqual> <operation>
  | <app>
  | <term>

<signature> ::= <SEIdentifier> <SEColon> <simplExpr>

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
 | Unit
<AppTerm> ::= <Application>
<IntegerLiteral> ::= Integer
<DoubleLiteral> ::= Double
<StringLiteral> ::= String
<IdTerm> ::= <Identifier>
<ArwTerm> ::= <Arrow>

<Application> ::= <SimpleExpr> <SimpleExpr>

<Identifier> ::= String

<Arrow> ::=
    <SimpleExpr> <SimpleExpr>
  | <Signature> <SimpleExpr>

<Bracket> ::=
    <Signature> <SimpleExpr>

<Signature> ::= <Identifier> <SimpleExpr>
-}

-- ---data type defentions---


mutual
    -- the data type for AST of SimpleExpr
    -- <SimpleExpr> ::=
    --    <IdTerm>
    --  | <AppTerm>
    --  | <ArwTerm>
    --  | <IntegerLiteral>
    --  | <DoubleLiteral>
    --  | <StringLiteral>
    --  | <CharLiteral>
    --  | <PrTerm>
    --  | Unit
    -- <AppTerm> ::= <Application>
    -- <IntegerLiteral> ::= Integer
    -- <DoubleLiteral> ::= Double
    -- <StringLiteral> ::= String
    -- <IdTerm> ::= <Identifier>
    -- <ArwTerm> ::= <Arrow>
    -- <PrTerm> ::= <Pair>
    public export
    data SimpleExprProt : Type ->Type where
        IdTerm : {0 id : Type} -> IdentifierProt id -> SimpleExprProt id
        AppTerm : {0 id : Type} -> ApplicationProt id -> SimpleExprProt id
        ArwTerm : {0 id : Type} -> ArrowProt id False -> SimpleExprProt id
        DArwTerm : {0 id : Type} -> DArrowProt id False -> SimpleExprProt id
        BArwTerm : {0 id : Type} -> BracketArwProt id -> SimpleExprProt id
        IntegerLiteral : {0 id : Type} -> Integer -> SimpleExprProt id
        DoubleLiteral : {0 id : Type} -> Double -> SimpleExprProt id
        CharLiteral : {0 id : Type} -> Char -> SimpleExprProt id
        StringLiteral : {0 id : Type} -> String -> SimpleExprProt id
        UnitTerm : {0 id : Type} -> SimpleExprProt id

    -- <Identifier> ::= String
    public export
    data IdentifierProt : Type -> Type where
        MkId : {0 id : Type} -> id -> IdentifierProt id

    -- <Application> ::= <SimpleExpr> <SimpleExpr>
    public export
    data ApplicationProt : Type -> Type where
        MkApp : {0 id : Type} -> SimpleExprProt id -> SimpleExprProt id -> ApplicationProt id

    -- Arrow True eliminates the pattern of SiExArr
    -- <Arrow> ::=
    --     <SimpleExpr> <SimpleExpr>
    --   | <Signature> <SimpleExpr>
    public export
    data ArrowProt : Type -> (nosig : Bool) -> Type where
        ExExArr : {0 id : Type} -> {0 b : Bool} -> SimpleExprProt id -> SimpleExprProt id -> ArrowProt id b
        SiExArr : {0 id : Type} -> SignatureProt id -> SimpleExprProt id -> ArrowProt id False

    public export
    data DArrowProt : Type -> (nosig : Bool) -> Type where
        ExExDArr : {0 id : Type} -> {0 b : Bool} -> SimpleExprProt id -> SimpleExprProt id -> DArrowProt id b
        SiExDArr : {0 id : Type} -> SignatureProt id -> SimpleExprProt id -> DArrowProt id False

    -- <BracketSignature> ::= <Identifier> <SimpleExpr>
    public export
    data BracketArwProt : Type -> Type where
      MkBracket : {0 id : Type} -> SignatureProt id -> SimpleExprProt id -> BracketArwProt id

    -- <Signature> ::= <Identifier> <SimpleExpr>
    public export
    data SignatureProt : Type -> Type where
      MkSignature : {0 id : Type} -> (name : IdentifierProt String) -> SimpleExprProt id -> SignatureProt id



public export
SimpleExpr : Type
SimpleExpr = SimpleExprProt String

public export
Identifier : Type
Identifier = IdentifierProt String

public export
Application : Type
Application = ApplicationProt String

public export
Arrow : Bool -> Type
Arrow = ArrowProt String

public export
DArrow : Bool -> Type
DArrow = DArrowProt String

public export
Signature : Type
Signature = SignatureProt String

public export
BracketArw : Type
BracketArw = BracketArwProt String

-- Arrow 
export
forgetSig : {0 id : Type} -> ArrowProt id b -> ArrowProt id True
forgetSig (ExExArr x y) = ExExArr x y
forgetSig (SiExArr (MkSignature str x) y) = ExExArr x y

export
forgetSigD : {0 id : Type} -> DArrowProt id b -> DArrowProt id True
forgetSigD (ExExDArr x y) = ExExDArr x y
forgetSigD (SiExDArr (MkSignature str x) y) = ExExDArr x y

export
nameeq : String -> String -> Bool
nameeq str1 str2 = 
  case (split (\x => x == '.') str1, split (\x => x == '.') str2) of
    ((head1 ::: []), (head2 ::: [])) => head1 == head2
    (list@(head1 ::: (y :: xs)), (head2 ::: [])) => last list == head2
    ((head1 ::: []), list@(head2 ::: (y :: xs))) => head2 == last list
    (list1@(head1 ::: (z :: ys)), list2@(head2 ::: (y :: xs))) => list1 == list2

export
Eq Identifier where
    (==) (MkId str) (MkId str1) = nameeq str str1

mutual
    export
    exprEquality : {0 id : Type} -> Eq (IdentifierProt id) => SimpleExprProt id -> SimpleExprProt id -> Bool
    exprEquality (IdTerm x) (IdTerm y) = x == y
    exprEquality (AppTerm x) (AppTerm y) = appEquality x y
    exprEquality (ArwTerm x) (ArwTerm y) = sameTypeArw x y
    exprEquality (DArwTerm x) (DArwTerm y) = ?rhs
    exprEquality (BArwTerm x) (BArwTerm y) = ?rhs2
    exprEquality (IntegerLiteral k1) (IntegerLiteral k2) = k1 == k2
    exprEquality (DoubleLiteral dbl1) (DoubleLiteral dbl2) = dbl1 == dbl2
    exprEquality (StringLiteral str1) (StringLiteral str2) = str1 == str2
    exprEquality UnitTerm UnitTerm = True
    exprEquality _ _ = False

    appEquality : {0 id : Type} -> Eq (IdentifierProt id) => ApplicationProt id -> ApplicationProt id -> Bool
    appEquality (MkApp x z) (MkApp y w) = exprEquality x y && exprEquality z w


    sameTypeArw : {0 id : Type} -> Eq (IdentifierProt id) => ArrowProt id b1 -> ArrowProt id b2 -> Bool
    sameTypeArw (ExExArr x z) (ExExArr y w) = exprEquality x z && exprEquality y w
    sameTypeArw (ExExArr x z) (SiExArr y w) =
      let MkSignature _ yex = y in 
        exprEquality x z && exprEquality yex w
    sameTypeArw (SiExArr x z) (ExExArr y w) =
      let MkSignature _ xex = x in 
        exprEquality xex z && exprEquality y w
    sameTypeArw (SiExArr x z) (SiExArr y w) =
      let 
        MkSignature _ xex = x
        MkSignature _ yex = y
      in 
        exprEquality xex z && exprEquality yex w

export
Show Identifier where
  show (MkId str) =
    last $ split (\x => x == '.') str
      

mutual
  export
  showSimpleExpr : {0 id : Type} -> Show (IdentifierProt id) => Nat -> SimpleExprProt id -> String
  showSimpleExpr d (IdTerm id@(MkId name)) = show id
  showSimpleExpr d (AppTerm x) = showApp d x
  showSimpleExpr d (ArwTerm x) = showArw d x
  showSimpleExpr d (DArwTerm x) = showDArw d x
  showSimpleExpr d (BArwTerm x) = showBArw d x
  showSimpleExpr d (IntegerLiteral k) = show k
  showSimpleExpr d (DoubleLiteral dbl) = show dbl
  showSimpleExpr d (CharLiteral c) = show c
  showSimpleExpr d (StringLiteral str) = show str
  showSimpleExpr d UnitTerm = "()"

  showApp :{0 id : Type} -> Show (IdentifierProt id) =>  Nat -> ApplicationProt id -> String
  showApp d (MkApp x y) = showParens (d >= 3) $ showSimpleExpr 2 x ++ " " ++ showSimpleExpr 3 y

  showArw : {0 id : Type} -> Show (IdentifierProt id) => Nat -> ArrowProt id False -> String
  showArw d (ExExArr x y) = showParens (d >= 2) $ showSimpleExpr 2 x ++ " -> " ++ showSimpleExpr 1 y
  showArw d (SiExArr (MkSignature name x) y) = showParens (d >= 2) $ "(" ++ show name ++ " : " ++ showSimpleExpr 0 x ++ ") -> " ++ showSimpleExpr 2 y

  showDArw : {0 id : Type} -> Show (IdentifierProt id) => Nat -> DArrowProt id False -> String
  showDArw d (ExExDArr x y) = showParens (d >= 2) $ showSimpleExpr 2 x ++ " => " ++ showSimpleExpr 1 y
  showDArw d (SiExDArr (MkSignature name x) y) = showParens (d >= 2) $ "(" ++ show name ++ " : " ++ showSimpleExpr 0 x ++ ") => " ++ showSimpleExpr 2 y


  showBArw : {0 id : Type} -> Show (IdentifierProt id) => Nat -> BracketArwProt id -> String
  showBArw d (MkBracket (MkSignature name x) y) = showParens (d >= 2) $ "{" ++ show name ++ " : " ++ showSimpleExpr 0 x ++ "} -> " ++ showSimpleExpr 2 y


  --| keep this one because someday we will consider about resugaring like these |--
  -- showPr : {0 id : Type} -> Show (IdentifierProt id) => Nat -> PairProt id -> String
  -- showPr d x = "(" ++ showPrNoBracket x ++ ")"
  -- showPrNoBracket : {0 id : Type} -> Show (IdentifierProt id) => PairProt id -> String
  -- showPrNoBracket (MkPair x (PrTerm y)) = showSimpleExpr 0 x ++ ", " ++ showPrNoBracket y
  -- showPrNoBracket (MkPair x y) = showSimpleExpr 0 x ++ ", " ++ showSimpleExpr 0 y

export
%hint
ShowSimpleExprProtImpl : {0 id : Type} -> Show (IdentifierProt id) => Show (SimpleExprProt id)
ShowSimpleExprProtImpl = MkShow (\x => showSimpleExpr 0 x) (\_, x => showSimpleExpr 0 x)
  -- show x = showSimpleExpr 0 x

export
Show Signature where
  show (MkSignature name x) = show name ++ " : " ++ showSimpleExpr 0 x

-- information of operator used for parsing
public export
data OpRecord = MkOpRecord String Nat Assoc

public export
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
    pure $ \x,y => AppTerm $ (MkApp (AppTerm $ MkApp (IdTerm $ MkId "Equal") x)) y

-- this is parsed using optable
appOp : Grammar state SimpleExprToken True (SimpleExpr -> SimpleExpr -> SimpleExpr)
appOp =
  do
    match SEDollar
    pure $ \x, y => AppTerm $ MkApp x y

-- the main parser
-- starts in expr
mutual
    -- <simpleExpr> ::=
    --     <arrow>
    --   | <operation>
    simpleExpr : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    simpleExpr optable =
      do
        map ArwTerm (arrow optable)
      <|>
        map DArwTerm (darrow optable)
      <|>
        map BArwTerm (barrow optable)
      <|>
        operation optable

    pair : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    pair optable = 
      do
        match SELParen
        p <- pairSub optable
        match SERParen
        pure p

    pairSub : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True SimpleExpr
    pairSub optable =
      do
        e <- simpleExpr optable
        match SEComma
        p <- pairSub optable
        pure $ AppTerm $ MkApp (AppTerm $ MkApp (IdTerm $ MkId "Pair") e) p 
      <|>
      do
        e1 <- simpleExpr optable
        match SEComma
        e2 <- simpleExpr optable
        pure $ AppTerm $ MkApp (AppTerm $ MkApp (IdTerm $ MkId "Pair") e1) e2

    -- <arrow> ::= 
    --   | <operation> <SEArrow> <expr>
    --   | <SELParen> <signature> <SERParen> <SEArrow> <expr>
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
    
    darrow : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True (DArrow False)
    darrow optable =
      do
        e1 <- operation optable
        match SEDoubleArrow
        e2 <- simpleExpr optable
        pure $ ExExDArr e1 e2
      <|>
      do
        match SELParen
        sig <- signature optable
        match SERParen
        match SEDoubleArrow
        e <- simpleExpr optable
        pure $ SiExDArr sig e

    barrow : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True BracketArw
    barrow optable = 
      do
        match SELBracket
        sig <- signature optable
        match SERBracket
        match SEArrow
        e <- simpleExpr optable
        pure $ MkBracket sig e

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
        buildExpressionParser (optable ++ [[Infix equality AssocNone, Infix appOp AssocRight]]) (map AppTerm (app optable) <|> term optable)
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
    --     <identifier> <term> <appSub1> 
    --   | <identifier> <term>
    --   | <appWithParen>
    -- <appSub1> ::= <appSub2> | ε
    -- <appSub2> ::= <term> <appSub1>
    app : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True Application
    app optable =
      -- the first two sytax corresponds to this part
      do
        id <- identifier
        t <- term optable
        appSub1 optable $ MkApp (IdTerm id) t
      <|>
      do
        a <- paren optable
        t <- term optable
        appSub1 optable (MkApp a t)

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
        pair optable
        -- map PrTerm $ pair optable
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
    --   | <SECharLiteral>
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
        c <- match SECharLiteral
        pure $ CharLiteral c
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



export
opTable : InOperatorMap -> OperatorTable state SimpleExprToken SimpleExpr
opTable opmap = dynOperatorTable opmap

-- parses token list
parseSimpleExpr : InOperatorMap -> List (WithBounds SimpleExprToken) -> Either String SimpleExpr
parseSimpleExpr opmap toks =
  case parse (simpleExpr $ opTable opmap) $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left $ show xs -- Left "contains tokens that were not consumed"
    Left e => Left (show e)

-- parses string to AST
export
parse : InOperatorMap -> String -> Either String SimpleExpr
parse opmap x =
  case lexSimpleExpr x of
    Just toks => parseSimpleExpr opmap toks
    Nothing => Left "Failed to lex."


