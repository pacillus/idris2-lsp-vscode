module Pacillus.Idris2LSP.Parser.Parse

import Data.List
import Text.Parser
import Text.Parser.Expression

import Pacillus.Idris2LSP.Parser.Basic
import Pacillus.Idris2LSP.Parser.Lexer

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
simpleExprInf2App : String -> Sugared Expr -> Sugared Expr -> Sugared Expr
simpleExprInf2App inid t1 t2 =
  let
    infixid = IdTerm (MkId inid)
    firstapp = AppTerm $ MkApp infixid t1
  in
    AppTerm $ MkApp firstapp t2


-- <infixOperator> ::= <SESymbol>
infixOperator : (symbol_name : String) -> Grammar state SimpleExprToken True (Sugared Expr -> Sugared Expr -> Sugared Expr)
infixOperator symbol_name =
  do
    sym <- match SESymbol
    when (sym /= symbol_name) $ fail "not a matching operator" -- only parses the symbol of arg
    pure $ simpleExprInf2App $ "(" ++ sym ++ ")"

-- <infixFunction> ::= <SEBackquote> <SEIdentifier> <SEBackquote>
infixFunction : Grammar state SimpleExprToken True (Sugared Expr -> Sugared Expr -> Sugared Expr)
infixFunction =
  do
    match SEBackquote
    id <- match SEIdentifier
    match SEBackquote
    pure $ simpleExprInf2App id

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
    pure $ \x,y => AppTerm $ (MkApp (AppTerm $ MkApp (IdTerm $ MkId "Equal") x)) y

-- this is parsed using optable
appOp : Grammar state SimpleExprToken True (Sugared Expr -> Sugared Expr -> Sugared Expr)
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
    simpleExpr : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    simpleExpr = tArrows

    tArrows : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    tArrows optable =
      do
        map ArwTerm (arrow optable)
      <|>
        map DArwTerm (darrow optable)
      <|>
        map BArwTerm (barrow optable)
      <|>
        tOperators optable

    tOperators : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    tOperators optable = operation optable

    tApp : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Expr)
    tApp optable = map AppTerm (app optable) <|> term optable

    -- <signature> ::= <SEIdentifier> <SEColon> <SimpleExpr>
    export
    signature : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Sugared Sig)
    signature optable = 
      do
        id <- match SEIdentifier
        match SEColon
        e <- tArrows optable
        pure $ MkSignature (MkId id) e

    -- <arrow> ::= 
    --   | <operation> <SEArrow> <expr>
    --   | <SELParen> <signature> <SERParen> <SEArrow> <expr>
    arrow : OperatorTable state SimpleExprToken (Sugared Expr) -> Grammar state SimpleExprToken True (Arrow False)
    arrow optable =
      do
        e1 <- tOperators optable
        match SEArrow
        e2 <- tArrows optable
        pure $ ExExArr e1 e2
      <|>
      do
        match SELParen
        sig <- signature optable
        match SERParen
        match SEArrow
        e <- tArrows optable
        pure $ SiExArr sig e
    
    darrow : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True (DArrow False)
    darrow optable =
      do
        e1 <- tOperators optable
        match SEDoubleArrow
        e2 <- tArrows optable
        pure $ ExExDArr e1 e2
      <|>
      do
        match SELParen
        sig <- signature optable
        match SERParen
        match SEDoubleArrow
        e <- tArrows optable
        pure $ SiExDArr sig e

    barrow : OperatorTable state SimpleExprToken SimpleExpr -> Grammar state SimpleExprToken True BracketArw
    barrow optable = 
      do
        match SELBracket
        sig <- signature optable
        match SERBracket
        match SEArrow
        e <- tArrows optable
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
        buildExpressionParser (optable ++ [[Infix equality AssocNone, Infix appOp AssocRight]]) (tApp optable)
      <|>
        term optable

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

    -- <identifier> ::= <SEIdentifier>
    identifier : Grammar state SimpleExprToken True Identifier
    identifier =
        map MkId (match SEIdentifier)
      <|>
      do
        match SELParen
        id <- identifier
        match SERParen
        pure id -- TODO do we need this?

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
        e <-  tArrows optable
        match SERParen
        pure e



export
opTable : InOperatorMap -> OperatorTable state SimpleExprToken (Sugared Expr)
opTable opmap = dynOperatorTable opmap

-- parses token list
parseSimpleExpr : InOperatorMap -> List (WithBounds SimpleExprToken) -> Either String (Sugared Expr)
parseSimpleExpr opmap toks =
  case parse (simpleExpr $ opTable opmap) $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left $ show xs -- Left "contains tokens that were not consumed"
    Left e => Left (show e)

-- parses string to AST
export
parse : InOperatorMap -> String -> Either String Sugared
parse opmap x =
  case lexSimpleExpr x of
    Just toks => parseSimpleExpr opmap toks
    Nothing => Left "Failed to lex."