module Pacillus.Idris2LSP.Parser.Sugared

import Data.List
import Text.Parser
import Text.Parser.Expression

import Pacillus.Idris2LSP.Parser.Basic
import Pacillus.Idris2LSP.Parser.Lexer

-- this program was made referencing "Documentation for the Idris 2 Language/Cookbook/Parsing"
-- https://idris2.readthedocs.io/en/latest/cookbook/parsing.html

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

-- ---Parsers---
-- <infixOperator> ::= <SESymbol>
infixOperator : (symbol_name : String) -> Grammar state SimpleExprToken True (Sugared Expr -> Sugared Expr -> Sugared Expr)
infixOperator symbol_name =
  do
    sym <- match SESymbol
    when (sym /= symbol_name) $ fail "not a matching operator" -- only parses the symbol of arg
    pure $ \e1, e2 => OpInfixSugar e1 (MkOperator sym) e2

-- <infixFunction> ::= <SEBackquote> <SEIdentifier> <SEBackquote>
infixFunction : Grammar state SimpleExprToken True (Sugared Expr -> Sugared Expr -> Sugared Expr)
infixFunction =
  do
    match SEBackquote
    id <- match SEIdentifier
    match SEBackquote
    pure $ \e1, e2 => InfixSugar e1 (MkIdentifier NameId id) e2

-- dynnamically constructs a OperatorTable for parsing expr
dynOperatorTable : InOperatorMap -> OperatorTable state SimpleExprToken (Sugared Expr)
dynOperatorTable opmap =
  let
    -- below is the data flow
    -- opmap -> norm_opmap -> mergerd_norm -> grouped -> sorted -> return!
    -- type for normalized Operator information. state is passed by arg
    OpNorm : Type -> Type
    OpNorm state = (Nat, Op state SimpleExprToken $ Sugared Expr)

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
    forgetAll : List1 (OpNorm state) -> List (Op state SimpleExprToken (Sugared Expr))
    forgetAll xs = map snd $ forget xs
  in
    map forgetAll sorted

-- this is parsed using optable
equality : Grammar state SimpleExprToken True (Sugared Expr -> Sugared Expr -> Sugared Expr)
equality =
  do
    match SEEqual
    pure $ \x,y => EqualSugar x y

    -- pure $ \x,y => AppTerm $ (MkApp (AppTerm $ MkApp (IdTerm $ MkId "Equal") x)) y

-- this is parsed using optable
appOp : Grammar state SimpleExprToken True (Sugared Expr -> Sugared Expr -> Sugared Expr)
appOp =
  do
    match SEDollar
    pure $ \x, y => Application x y

-- the main parser
-- starts in top
mutual
    -- <simpleExpr> ::=
    --     <arrow>
    --   | <operation>
    top : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    top = tArrows

    tArrows : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    tArrows optable =
        arrow optable
      <|>
        darrow optable
      <|>
        barrow optable
      <|>
        dependentPair optable
      <|>
        dependentPairConstructor optable
      <|>
        anonymousFunction optable
      <|>
        tOperators optable

    tOperators : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    tOperators optable = operation optable

    tApp : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    tApp optable = app optable <|> memberOrAtom optable

    -- <dependentPair> ::= <SELParen> <identifier> <SEColon> <tArrows> <SEDoubleStar> <tDepPair> <SERParen>
    dependentPair : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    dependentPair optable = 
      do
        match SELParen
        id <- map (MkIdentifier NameId) (match SEIdentifier)
        match SEColon
        ty <- tArrows optable
        match SEDoubleStar
        e <- tArrows optable
        match SERParen
        pure $ DependentPairSugar id ty e
    -- <dependentPairConstructor>
    dependentPairConstructor : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    dependentPairConstructor optable = 
      do
        match SELParen
        e1 <- tArrows optable
        match SEDoubleStar
        e2 <- tArrows optable
        match SERParen
        pure $ DependentPairConstructorSugar e1 e2

    -- <signature> ::= <SEIdentifier> <SEColon> <SimpleExpr>
    export
    signature : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Sig)
    signature optable = 
      do
        id <- identifier
        match SEColon
        e <- tArrows optable
        pure $ Signature id e

    ignoreZero : Grammar state SimpleExprToken False ()
    ignoreZero = 
      (>>=) {c2 = False} (match SEIntLiteral)  (
          \n =>
            if n == 0
              then Core.pure {state, tok = SimpleExprToken} ()
              else fail {state, c = False} "non zero Type")
      <|>
        pure ()

    ignoreUnderscoreBinder : Grammar state SimpleExprToken False ()
    ignoreUnderscoreBinder =
      do
        match SEWildcard
        match SEColon
        pure ()
      <|>
        pure()

    -- <arrow> ::= 
    --   | <operation> <SEArrow> <expr>
    --   | <SELParen> <signature> <SERParen> <SEArrow> <expr>
    arrow : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    arrow optable =
      do
        ignoreZero
        e1 <- tOperators optable
        match SEArrow
        e2 <- tArrows optable
        pure $ Arrow SingleLine e1 e2
      <|>
      do
        match SELParen
        ignoreZero
        id <- identifier
        match SEColon
        ty <- tArrows optable
        match SERParen
        match SEArrow
        e <- tArrows optable
        pure $ SignatureArrow SingleLine id ty e
      <|>
      do
        match SELParen
        ignoreZero
        match SEWildcard
        match SEColon
        ty <- tArrows optable
        match SERParen
        match SEArrow
        e <- tArrows optable
        pure $ SignatureArrow SingleLine (MkIdentifier NameId "_") ty e
    
    darrow : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    darrow optable =
      do
        ignoreZero
        ignoreUnderscoreBinder
        e1 <- tOperators optable
        match SEDoubleArrow
        e2 <- tArrows optable
        pure $ Arrow DoubleLine e1 e2
      <|>
      do
        match SELParen
        ignoreZero
        id <- identifier
        match SEColon
        ty <- tArrows optable
        match SERParen
        match SEDoubleArrow
        e <- tArrows optable
        pure $ SignatureArrow DoubleLine id ty e
      <|>
      do
        match SELParen
        ignoreZero
        match SEWildcard
        match SEColon
        ty <- tArrows optable
        match SERParen
        match SEDoubleArrow
        e <- tArrows optable
        pure $ SignatureArrow DoubleLine (MkIdentifier NameId "_") ty e

    barrow : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    barrow optable =
      do
        ignoreZero
        match SELBracket -- {
        id <- identifier
        match SEColon
        ty <- tArrows optable
        match SERBracket
        match SEArrow
        e <- tArrows optable
        pure $ BracketArrow id ty e
      <|>
      do
        match SELBracket
        ignoreZero
        match SEWildcard
        match SEColon
        ty <- tArrows optable
        match SERBracket
        match SEArrow
        e <- tArrows optable
        pure $ BracketArrow (MkIdentifier NameId "_") ty e

    anonymousFunction : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    anonymousFunction optable = 
      do
        match SEBackslash
        id <- map (MkIdentifier NameId) (match SEIdentifier)
        match SEDoubleArrow
        e <-  top optable
        pure $ AnonymousFunction id e

    -- specially parsed using optable
    -- includes infix function, infix operation, and equality
    -- <operation>
    --   | <operation> <infixOperator> <operation>
    --   | <operation> <infixFunction> <operation>
    --   | <operation> <SEEqual> <operation>
    --   | <app>
    --   | <term>
    operation : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    operation optable =
        buildExpressionParser (optable ++ [[Infix equality AssocNone, Infix appOp AssocRight]]) (tApp optable)
      <|>
        memberOrAtom optable -- TODO do we need this

    -- left most part of application must be a identifier
    -- <app> ::=
    --     <identifier> <term> <appSub1> 
    --   | <identifier> <term>
    --   | <appWithParen>
    -- <appSub1> ::= <appSub2> | ε
    -- <appSub2> ::= <term> <appSub1>
    app : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    app optable =
      -- the first two sytax corresponds to this part
      do
        t1 <- memberOrAtom optable
        t2 <- memberOrAtom optable
        appSub1 optable $ Application t1 t2

    -- subfunction for app
    appSub1 : OperatorTable state SimpleExprToken (Sugared Expr) -> Sugared Expr -> Grammar state SimpleExprToken False (Sugared Expr)
    appSub1 optable e = appSub2 optable e <|> pure e

    -- subfunction for app
    appSub2 : OperatorTable state SimpleExprToken (Sugared Expr) -> Sugared Expr -> Grammar state SimpleExprToken True (Sugared Expr)
    appSub2 optable app = 
      do
        t <- memberOrAtom optable
        appSub1 optable $ Application app t

    memberOrAtom : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    memberOrAtom optable =
      do
        e <- atom optable
        memberOrAtomSub optable e

    
    memberOrAtomSub : OperatorTable state SimpleExprToken (Sugared Expr) -> Sugared Expr -> Grammar state SimpleExprToken False (Sugared Expr)
    memberOrAtomSub optable f =
      do
        x <- match SEMember
        memberOrAtomSub optable $ MemberSugar f (MkMember x)
      <|>
        pure f

    -- <term> ::=
    --     <unit>
    --     <pair>
    --     <var>
    --   | <literal>
    --   | <paren>
    atom : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    atom optable =
      do
        match SELParen
        match SERParen
        pure UnitSugar
      <|> pair optable
      <|> map IdentifierTerm identifier
      <|> map HoleTerm hole
      <|> literal
      <|> wildcard
      <|> paren optable
      -- <|> member optable

    pair : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    pair optable = 
      do
        match SELParen
        p <- pairSub optable
        match SERParen
        pure p

    pairSub : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    pairSub optable =
      do
        e <- top optable
        match SEComma
        p <- pairSub optable
        pure $ PairSugar e p
      <|>
      do
        e1 <- top optable
        match SEComma
        e2 <- top optable
        pure $ PairSugar e1 e2

    -- <identifier> ::= <SEIdentifier>
    identifier : Grammar state SimpleExprToken True Identifier
    identifier =
        map (MkIdentifier NameId) (match SEIdentifier)
      <|>
        map (MkIdentifier OperatorId) (match SEOperator)
      <|>
        map (MkIdentifier MemberId) (match SEMemberId)
    
    hole : Grammar state SimpleExprToken True Hole
    hole = map MkHole (match SEHole)

    -- <literal> ::=
    --     <SEIntLiteral>
    --   | <SEDoubleLiteral>
    --   | <SECharLiteral>
    --   | <SEStringLiteral>
    literal : Grammar state SimpleExprToken True (Sugared Expr)
    literal =
      do
        n <- match SEIntLiteral
        pure $ Literal IntegerL n
      <|>
      do
        n <- match SEDoubleLiteral
        pure $ Literal DoubleL n
      <|>
      do
        c <- match SECharLiteral
        pure $ Literal CharL c
      <|>
      do
        s <- match SEStringLiteral
        pure $ Literal StringL s
      
    -- <wildcard> ::= <SEWildcard>
    wildcard : Grammar state SimpleExprToken True (Sugared Expr)
    wildcard = 
      do
        match SEWildcard
        pure $ Wildcard

    -- <paren> ::= <SELParen> <simplExpr> <SERParen> 
    paren : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    paren optable =
      do
        match SELParen
        e <-  top optable
        match SERParen
        pure e



export
opTable : InOperatorMap -> OperatorTable state SimpleExprToken (Sugared Expr)
opTable opmap = dynOperatorTable opmap

-- parses token list
parseSimpleExpr : InOperatorMap -> List (WithBounds SimpleExprToken) -> Either String (Sugared Expr)
parseSimpleExpr opmap toks =
  case parse (top $ opTable opmap) $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left $ show xs -- Left "contains tokens that were not consumed"
    Left e => Left (show e)

parseSignature : InOperatorMap -> List (WithBounds SimpleExprToken) -> Either String (Sugared Sig)
parseSignature opmap toks =
  case parse (signature $ opTable opmap) $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left $ show xs -- Left "contains tokens that were not consumed"
    Left e => Left (show e)

-- parses string to AST
export
parse : InOperatorMap -> String -> Either String (Sugared Expr)
parse opmap x =
  case lexSimpleExpr x of
    Just toks => parseSimpleExpr opmap toks
    Nothing => Left "Failed to lex."

export
parseSig : InOperatorMap -> String -> Either String (Sugared Sig)
parseSig opmap str with (lexSimpleExpr str)
  parseSig opmap str | Nothing = Left "Failed to lex."
  parseSig opmap str | Just toks = parseSignature opmap toks
