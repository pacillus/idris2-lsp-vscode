module Pacillus.Idris2LSP.TypeRecons.TypeRecons

import Data.String
import Text.Parser
import Text.Parser.Expression

import Pacillus.Idris2LSP.Syntax.SimpleExpr
import Pacillus.Idris2LSP.Syntax.Lexer

%default total

data LabeledIdentifier = Variable String | Constant String

SimpleExprU : Type
SimpleExprU = SimpleExprProt LabeledIdentifier

IdentifierU : Type
IdentifierU = IdentifierProt LabeledIdentifier

ApplicationU : Type
ApplicationU = ApplicationProt LabeledIdentifier


ArrowU : Bool -> Type
ArrowU = ArrowProt LabeledIdentifier

DArrowU : Bool -> Type
DArrowU = DArrowProt LabeledIdentifier

BArrowU : Type
BArrowU = BracketArwProt LabeledIdentifier

SignatureU : Type
SignatureU = SignatureProt LabeledIdentifier

forgetLabel : LabeledIdentifier -> String
forgetLabel (Variable str) = str
forgetLabel (Constant str) = str

Constraints : Type
Constraints = List (SimpleExprU, SimpleExprU)

Eq IdentifierU where
  MkId (Variable str1) == MkId (Variable str2) = nameeq str1 str2
  MkId (Constant str1) == MkId (Constant str2) = nameeq str1 str2
  _ == _ = False

Show IdentifierU where
  show (MkId (Variable str)) = "?" ++ str
  show (MkId (Constant str)) = str

toTypeVar : Identifier -> IdentifierU
toTypeVar (MkId x) = MkId (Variable x)


allAsConstant : SimpleExpr -> SimpleExprU
allAsConstant (IdTerm (MkId x)) = IdTerm (MkId (Constant x))
allAsConstant (AppTerm (MkApp x y)) = AppTerm (MkApp (allAsConstant x) (allAsConstant y))
allAsConstant (ArwTerm (ExExArr x y)) = ArwTerm (ExExArr (allAsConstant x) (allAsConstant y))
allAsConstant (ArwTerm (SiExArr (MkSignature name x) y)) = 
  ArwTerm $ SiExArr (MkSignature name (allAsConstant x)) (allAsConstant y)
allAsConstant (DArwTerm (ExExDArr x y)) = DArwTerm (ExExDArr (allAsConstant x) (allAsConstant y))
allAsConstant (DArwTerm (SiExDArr (MkSignature name x) y)) = 
  DArwTerm $ SiExDArr (MkSignature name (allAsConstant x)) (allAsConstant y)
allAsConstant (BArwTerm (MkBracket (MkSignature name x) y)) = 
  BArwTerm $ MkBracket (MkSignature name (allAsConstant x)) (allAsConstant y)
allAsConstant (IntegerLiteral i) = IntegerLiteral i
allAsConstant (DoubleLiteral dbl) = DoubleLiteral dbl
allAsConstant (CharLiteral c) = CharLiteral c
allAsConstant (StringLiteral str) = StringLiteral str
allAsConstant UnitTerm = UnitTerm



-- token to Signature AST
parseSignature : InOperatorMap -> List (WithBounds SimpleExprToken) -> Either String Signature
parseSignature opmap toks =
  case parse (signature $ opTable opmap) $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left $ "contains tokens that were not consumed\n" ++ show xs
    Left e => Left (show e)

-- string to Signature AST
export
parseSig : InOperatorMap -> String -> Either String Signature
parseSig opmap x with (lexSimpleExpr x)
  parseSig opmap x | Just toks = parseSignature opmap toks
  parseSig opmap x | Nothing = Left "Failed to lex."

-- lowercase and not applied to anything

-- variables from left and variables from right are separated on unification

-- normal signature is a signature where a single id is tied to the type
-- this signature is an original syntax where partial expression comes to the left part and it's type to the right
public export
data PartialExprSignature : Type where
    MkSpSig : (name : SimpleExpr) -> SimpleExprU -> PartialExprSignature

-- converts normal signature to spsig
export
toSpSig : SignatureU -> PartialExprSignature
toSpSig (MkSignature name x) = MkSpSig (IdTerm name) x

-- projects partial expression
getSpSigExpr : PartialExprSignature -> SimpleExprU
getSpSigExpr (MkSpSig name x) = x


public export
data ReconsTree : Type where
    Start : PartialExprSignature -> ReconsTree
    Subgoal : List ReconsTree -> PartialExprSignature -> ReconsTree

-- converts to normal signature then show
Show PartialExprSignature where
    show (MkSpSig name x) = showSimpleExpr 0 name ++ " : " ++ showSimpleExpr 0 x

covering export
Show ReconsTree where
    show (Start sig) = show sig
    show (Subgoal xs x) =
      let
        pres = map ((++) "| ") $ foldl (++) [] $ map (lines . show) xs
      in
        unlines $ pres ++ ["----------", show x]


getSubgoal : ReconsTree -> PartialExprSignature
getSubgoal (Start x) = x
getSubgoal (Subgoal xs x) = x

mutual
    assignExpr : (target : SimpleExprU)  -> (var : SimpleExprU) -> (replace : SimpleExprU) -> SimpleExprU
    assignExpr target var replace =
      if exprEquality target var
        then replace
        else
          case target of
            (IdTerm x) => IdTerm x
            (AppTerm x) => AppTerm $ assignApp x var replace
            (ArwTerm x) => ArwTerm $ assignArw x var replace
            (DArwTerm x) => DArwTerm $ assignDArw x var replace
            (BArwTerm x) => BArwTerm $ assignBArw x var replace
            (IntegerLiteral k) => IntegerLiteral k
            (DoubleLiteral dbl) => DoubleLiteral dbl
            (CharLiteral c) => CharLiteral c
            (StringLiteral str) => StringLiteral str
            UnitTerm => UnitTerm

    assignApp : (target : ApplicationU) -> (var : SimpleExprU) -> (replace : SimpleExprU) -> ApplicationU
    assignApp (MkApp x y) var replace = MkApp (assignExpr x var replace) (assignExpr y var replace)

    assignArw : (target : ArrowU b) -> (var : SimpleExprU) -> (replace : SimpleExprU) -> ArrowU b
    assignArw (ExExArr x y) var replace = ExExArr (assignExpr x var replace) (assignExpr y var replace)
    assignArw (SiExArr x y) var replace = SiExArr (assignSig x var replace) (assignExpr y var replace)

    assignDArw : (target : DArrowU b) -> (var : SimpleExprU) -> (replace : SimpleExprU) -> DArrowU b
    assignDArw (ExExDArr x y) var replace = ExExDArr (assignExpr x var replace) (assignExpr y var replace)
    assignDArw (SiExDArr x y) var replace = SiExDArr (assignSig x var replace) (assignExpr y var replace)

    assignBArw : (target : BArrowU) -> (var : SimpleExprU) -> (replace : SimpleExprU) -> BArrowU
    assignBArw (MkBracket x y) var replace = MkBracket (assignSig x var replace) (assignExpr y var replace)


    assignSig : (target : SignatureU) -> (var : SimpleExprU) -> (expr : SimpleExprU) -> SignatureU
    assignSig (MkSignature str x) var expr = MkSignature str $ assignExpr x var expr

assignL : (constraints : Constraints) -> (var : SimpleExprU) -> (expr : SimpleExprU) -> Constraints
assignL [] var expr = []
assignL ((x, y) :: xs) var expr = (assignExpr x var expr, y) :: assignL xs var expr

assignR : (constraints : Constraints) -> (var : SimpleExprU) -> (expr : SimpleExprU) -> Constraints
assignR [] var expr = []
assignR ((x, y) :: xs) var expr = (x, assignExpr y var expr) :: assignR xs var expr

isHeadLower : String -> Bool
isHeadLower str =
  case unpack str of
    [] => False
    (x :: xs) => isLower x

isImplicitVar : IdentifierU -> Bool
isImplicitVar (MkId (Variable str)) = True
isImplicitVar (MkId (Constant str)) = False

mutual
    data IsApplying = Applying | NotApplying

    0 LabelCondition : Type
    LabelCondition = IsApplying -> String -> Bool    

    labelImplicitExp : LabelCondition -> SimpleExprU -> SimpleExprU
    labelImplicitExp cond (IdTerm (MkId name)) =
      let
        name' = forgetLabel name 
      in
      if cond NotApplying name'
        then IdTerm $ MkId $ Variable name'
        else IdTerm $ MkId $ Constant name'
    labelImplicitExp cond (AppTerm x) = AppTerm $ labelImplicitApp cond x
    labelImplicitExp cond (ArwTerm x) = ArwTerm $ labelImplicitArw cond x
    labelImplicitExp cond (DArwTerm x) = DArwTerm $ labelImplicitDArw cond x
    labelImplicitExp cond (BArwTerm x) = BArwTerm $ labelImplicitBArw cond x
    labelImplicitExp cond (IntegerLiteral k) = IntegerLiteral k
    labelImplicitExp cond (DoubleLiteral dbl) = DoubleLiteral dbl
    labelImplicitExp cond (CharLiteral c) = CharLiteral c
    labelImplicitExp cond (StringLiteral str) = StringLiteral str
    labelImplicitExp cond UnitTerm = UnitTerm

    labelImplicitApp : LabelCondition  -> ApplicationU -> ApplicationU
    labelImplicitApp cond (MkApp (IdTerm (MkId name_s)) y) = 
      let
        name_s' = forgetLabel name_s
        -- with applying
        func =
          if cond Applying name_s'
            then IdTerm $ MkId $ Variable name_s'
            else IdTerm $ MkId $ Constant name_s'
      in
        MkApp func (labelImplicitExp cond y) 
    labelImplicitApp cond (MkApp x y) = MkApp (labelImplicitExp cond x) (labelImplicitExp cond y)

    labelImplicitArw : LabelCondition  -> ArrowU False -> ArrowU False
    labelImplicitArw cond (ExExArr x y) = ExExArr (labelImplicitExp cond x) (labelImplicitExp cond y)
    labelImplicitArw cond (SiExArr (MkSignature name@(MkId name_s) x) y) = SiExArr (MkSignature name (labelImplicitExp cond x)) (labelImplicitExp (\isapp, str => str /= name_s && cond isapp str) y)

    labelImplicitDArw : LabelCondition  -> DArrowU False -> DArrowU False
    labelImplicitDArw cond (ExExDArr x y) = ExExDArr (labelImplicitExp cond x) (labelImplicitExp cond y)
    labelImplicitDArw cond (SiExDArr (MkSignature name@(MkId name_s) x) y) = SiExDArr (MkSignature name (labelImplicitExp cond x)) (labelImplicitExp (\isapp, str => str /= name_s && cond isapp str) y)

    labelImplicitBArw : LabelCondition -> BArrowU -> BArrowU
    labelImplicitBArw cond (MkBracket (MkSignature name@(MkId name_s) x) y) = MkBracket (MkSignature name (labelImplicitExp cond x)) (labelImplicitExp (\isapp, str => str /= name_s && cond isapp str) y)

-- isHeadLower name && not ((MkId name) `elem` nimp)

normalcond : List Identifier -> LabelCondition
normalcond nimp Applying str = False
normalcond nimp NotApplying str = 
    isHeadLower str && not ((MkId str) `elem` nimp)

labelImplicitMain : List Identifier -> SimpleExpr -> SimpleExprU
labelImplicitMain nimp (ArwTerm (ExExArr x y)) = ArwTerm $ ExExArr (labelImplicitExp (normalcond nimp) (allAsConstant x)) (labelImplicitMain nimp y)
labelImplicitMain nimp (ArwTerm (SiExArr (MkSignature name x) y)) = ArwTerm $ SiExArr (MkSignature name $ labelImplicitExp (normalcond nimp) (allAsConstant x)) $ labelImplicitMain (name :: nimp) y
labelImplicitMain nimp x = labelImplicitExp (normalcond nimp) (allAsConstant x)

export
labelImplicit : SimpleExpr -> SimpleExprU
labelImplicit x = labelImplicitMain [] x

eqwithoutImp : SimpleExprU -> SimpleExprU -> Bool


mutual
    -- halt is guranteed due to the proof in "Types and Programming Languages" pg.327
    covering
    unify : Constraints -> Either String Constraints
    unify [] = Right []
    unify ((arg, app) :: xs) =
      if exprEquality arg app
        then unify xs
        else unifyExpr xs arg app

    showUniError : SimpleExprU -> SimpleExprU -> String
    showUniError x y = "unification failed between " ++ show x ++ " and " ++ show y

    covering
    unifyExpr : (constraints : Constraints) -> (arg : SimpleExprU) -> (applied : SimpleExprU) -> Either String Constraints
    unifyExpr constraints (IdTerm arg) (IdTerm applied) = unifyId constraints arg applied
    unifyExpr constraints (IdTerm arg) applied = unifyIdExpr constraints arg applied
    unifyExpr constraints arg (IdTerm applied) = unifyExprId constraints arg applied
    unifyExpr constraints (AppTerm arg) (AppTerm applied) = unifyApp constraints arg applied
    unifyExpr constraints (ArwTerm arg) (ArwTerm applied) = unifyArw constraints arg applied
    unifyExpr constraints (DArwTerm arg) (DArwTerm applied) = unifyDArw constraints arg applied
    unifyExpr _ arg applied = Left $ showUniError arg applied -- error
    
    covering
    unifyId : (constraints : Constraints) -> (arg : IdentifierU) -> (applied : IdentifierU) -> Either String Constraints
    unifyId constraints arg@(MkId (Variable str)) applied@(MkId str2) = uniVarExprL constraints (IdTerm arg) (IdTerm applied)
    unifyId constraints arg@(MkId (Constant str)) applied@(MkId (Variable str1)) = 
        uniVarExprR constraints (IdTerm arg) (IdTerm applied)
    unifyId constraints arg@(MkId (Constant str)) applied@(MkId (Constant str1)) = 
      if str == str1
        then unify constraints
        else Left $ showUniError (IdTerm arg) (IdTerm applied)
    
    covering
    unifyIdExpr : (constraints : Constraints) -> (arg : IdentifierU) -> (applied : SimpleExprU) -> Either String Constraints
    unifyIdExpr constraints arg@(MkId (Variable str)) applied = uniVarExprL constraints (IdTerm arg) applied
    unifyIdExpr constraints arg@(MkId (Constant str)) applied = Left $ showUniError (IdTerm arg) applied

    covering
    unifyExprId : (constraints : Constraints) -> (arg : SimpleExprU) -> (applied : IdentifierU) -> Either String Constraints
    unifyExprId constraints arg applied@(MkId (Variable str)) = uniVarExprR constraints arg (IdTerm applied)
    unifyExprId constraints arg applied@(MkId (Constant str)) = Left $ showUniError arg (IdTerm applied)

    covering
    unifyApp : (constraints : Constraints) -> (arg : ApplicationU) -> (applied : ApplicationU) -> Either String Constraints
    unifyApp constraints (MkApp x y) (MkApp z w) = unify $ (x, z) :: (y, w) :: constraints

    covering
    unifyArw : (constraints : Constraints) -> (arg : ArrowU False) -> (applied : ArrowU False) -> Either String Constraints
    unifyArw constraints (ExExArr x y) (ExExArr z w) = unify $ (x, z) :: (y, w) :: constraints
    unifyArw constraints (ExExArr x y) (SiExArr (MkSignature str z) w) = unify $ (x, z) :: (y, w) :: constraints
    unifyArw constraints (SiExArr (MkSignature str x) y) (ExExArr z w) = unify $ (x, z) :: (y, w) :: constraints
    unifyArw constraints (SiExArr (MkSignature name1 x) y) (SiExArr (MkSignature name2 z) w) = unify $ (x, z) :: (y, w) :: constraints

    covering
    unifyDArw : (constraints : Constraints) -> (arg : DArrowU False) -> (applied : DArrowU False) -> Either String Constraints
    unifyDArw constraints (ExExDArr x y) (ExExDArr z w) = unify $ (x, z) :: (y, w) :: constraints
    unifyDArw constraints (ExExDArr x y) (SiExDArr (MkSignature str z) w) = unify $ (x, z) :: (y, w) :: constraints
    unifyDArw constraints (SiExDArr (MkSignature str x) y) (ExExDArr z w) = unify $ (x, z) :: (y, w) :: constraints
    unifyDArw constraints (SiExDArr (MkSignature str x) y) (SiExDArr (MkSignature str1 z) w) = unify $ (x, z) :: (y, w) :: constraints

    covering
    uniVarExprL : (constraints : Constraints) -> (var : SimpleExprU) -> (expr : SimpleExprU) -> Either String Constraints
    uniVarExprL constraints var expr =
      do
        unified <- unify $ assignL constraints var expr
        pure $ (var, expr) :: unified

    covering
    uniVarExprR : (constraints : Constraints) -> (expr : SimpleExprU) -> (var : SimpleExprU) -> Either String Constraints
    uniVarExprR constraints expr var =
      do
        unified <- unify $ assignR constraints var expr
        pure $ (expr, var) :: unified

    

applyConstraints : Constraints -> SimpleExprU -> Either String SimpleExprU
applyConstraints [] x = Right x
applyConstraints ((y, z) :: xs) x =
  case (y, z) of
    (IdTerm var1, IdTerm var2) =>
        case (isImplicitVar var1, isImplicitVar var2) of
            (False, False) => Left "unexpected error: illegal constraints"
            (False, True) => applyConstraints xs $ assignExpr x (IdTerm var2) (IdTerm var1)
            (True, False) => applyConstraints xs $ assignExpr x (IdTerm var1) (IdTerm var2)
            (True, True) => applyConstraints xs x
    (IdTerm var, w) =>
      if isImplicitVar var
        then applyConstraints xs $ assignExpr x (IdTerm var) w
        else Left "unexpected error: illegal constraints"
    (w, IdTerm var) =>
      if isImplicitVar var
        then applyConstraints xs $ assignExpr x (IdTerm var) w
        else Left "unexpected error: illegal constraints"
    (_, _) => Left "unexpected error: illegal constraints"

export
getAppliedType : (f : PartialExprSignature) -> (x : PartialExprSignature) -> Either String PartialExprSignature
getAppliedType (MkSpSig name1 f) x with (f)
    getAppliedType (MkSpSig name1 f) (MkSpSig name2 x) | (ArwTerm f') =
      let
        result_name = MkApp name1 name2
        ExExArr arg ret = forgetSig f'
        assign : (target : SimpleExprU) -> (to : SimpleExpr) -> SimpleExprU
        assign target to =
          case f' of
            (ExExArr y z) => target
            (SiExArr (MkSignature name y) z) => assignExpr target (IdTerm $ toTypeVar name) (allAsConstant to)
      in
      do
        cons <- assert_total $ unify [(arg, x)]
        applied <- applyConstraints cons ret
        Right $ MkSpSig (AppTerm result_name) (assign applied name2)
    getAppliedType (MkSpSig name1 f) _ | _ = Left "left side not an appliable form"

-- 




-- TODO after implementing unification for interface, add from[LiteralType] to all literals
mutual
    -- get type of partial expression with its process using the signatures
    export
    getPartialType' : (sigs : List SignatureU) -> SimpleExpr -> Either String ReconsTree
    getPartialType' sigs (IdTerm x) = getIdType sigs x
    getPartialType' sigs (AppTerm x) = getAppType sigs x
    getPartialType' sigs (ArwTerm x) = assert_total $ getArwType sigs x -- in getArwType, value of x always decreases
    getPartialType' sigs (DArwTerm x) = assert_total $ getDArwType sigs x
    getPartialType' sigs (BArwTerm x) = assert_total $ getBArwType sigs x
    getPartialType' sigs (IntegerLiteral k) = Right $ Start $ MkSpSig (IdTerm $ MkId $ show k) $ IdTerm $ MkId $ Constant "Integer"
    getPartialType' sigs (DoubleLiteral dbl) = Right $ Start $ MkSpSig (IdTerm $ MkId $ show dbl) $ IdTerm $ MkId $ Constant "Double"
    getPartialType' sigs (CharLiteral c) = Right $ Start $ MkSpSig (IdTerm $ MkId $ show c) $ IdTerm $ MkId $ Constant "Char"
    getPartialType' sigs (StringLiteral str) = Right $ Start $ MkSpSig (IdTerm $ MkId $ show str) $ IdTerm $ MkId $ Constant "String"
    getPartialType' sigs UnitTerm =
      let
        sig = MkSpSig (IdTerm $ MkId "MkUnit") UnitTerm
      in
        Right $ Start sig

    getIdType : (sigs : List SignatureU) -> Identifier -> Either String ReconsTree
    getIdType [] y = Left $ "could not find the type of identifier " ++ show y
    getIdType (sig :: sigs) y =
      let
        MkSignature sig_id sig_expr = sig
      in
      if sig_id == y
        then Right $ Start (toSpSig sig)
        else getIdType sigs y

    -- sub function for getAppType
    constantToVariable : String -> SimpleExprU -> SimpleExprU
    constantToVariable str target = labelImplicitExp (\_, str2 => str2 == str) target

    -- sub function for getAppType
    skipImplicitType : PartialExprSignature -> PartialExprSignature
    skipImplicitType (MkSpSig name (DArwTerm $ ExExDArr expr1 expr2)) = MkSpSig name expr2
    skipImplicitType (MkSpSig name (DArwTerm $ SiExDArr (MkSignature (MkId name_s) x) expr)) = MkSpSig name (constantToVariable name_s expr)
    skipImplicitType (MkSpSig name (BArwTerm $ MkBracket (MkSignature (MkId name_s) x) expr)) = MkSpSig name (constantToVariable name_s expr)
    skipImplicitType sig = sig

    getAppType : (sigs : List SignatureU) -> Application -> Either String ReconsTree
    getAppType sigs (MkApp f x) =
      do
        ftree <- getPartialType' sigs f
        xtree <- getPartialType' sigs x
        appty <- getAppliedType (skipImplicitType $ getSubgoal ftree) (skipImplicitType $ getSubgoal $ xtree)
        pure $ Subgoal [ftree, xtree] appty

    getArwType : (sigs : List SignatureU) -> Arrow False -> Either String ReconsTree
    getArwType sigs x with (forgetSig x)
      getArwType sigs x | ExExArr argty retty = 
          let
            tycnst : SimpleExprU
            tycnst = IdTerm $ MkId $ Constant "Type"

            isType : PartialExprSignature -> Bool
            isType sig = exprEquality (getSpSigExpr sig) tycnst
          in
          do
            argsig <- getPartialType' sigs argty
            retsig <- getPartialType' sigs retty
            if isType (getSubgoal argsig) && isType (getSubgoal retsig)
                then Right (Subgoal [argsig, retsig] (MkSpSig (ArwTerm x) tycnst))
                else Left #"Found none "Type" identifier in arrow"#

    getDArwType : (sigs : List SignatureU) -> DArrow False -> Either String ReconsTree
    getDArwType sigs x with (forgetSigD x)
      getDArwType sigs x | ExExDArr argty retty = 
          let
            tycnst : SimpleExprU
            tycnst = IdTerm $ MkId $ Constant "Type"

            isType : PartialExprSignature -> Bool
            isType sig = exprEquality (getSpSigExpr sig) tycnst
          in
          do
            argsig <- getPartialType' sigs argty
            retsig <- getPartialType' sigs retty
            if isType (getSubgoal argsig) && isType (getSubgoal retsig)
                then Right (Subgoal [argsig, retsig] (MkSpSig (DArwTerm x) tycnst))
                else Left #"Found none "Type" identifier in darrow"#

    getBArwType : (sigs : List SignatureU) -> BracketArw -> Either String ReconsTree
    getBArwType sigs x@(MkBracket (MkSignature name argty) retty) =
      let
        tycnst : SimpleExprU
        tycnst = IdTerm $ MkId $ Constant "Type"

        isType : PartialExprSignature -> Bool
        isType sig = exprEquality (getSpSigExpr sig) tycnst
      in
      do
        argsig <- getPartialType' sigs argty
        retsig <- getPartialType' sigs retty
        if isType (getSubgoal argsig) && isType (getSubgoal retsig)
            then Right (Subgoal [argsig, retsig] (MkSpSig (BArwTerm x) tycnst))
            else Left #"Found none "Type" identifier in darrow"#

    getIntLitType : (sigs : List SignatureU) -> Integer -> Either String ReconsTree
    getIntLitType sigs k = ?rhs1

    -- getLitType : (sigs : List Signature) -> (fromlitty : PartialExprSignature) -> (literal : SimpleExpr) -> Either String ReconsTree
    -- getLitType sigs fromlitty literal = ?getLitType_rhs


-- PartialType
export
getPartialType : List Signature -> SimpleExpr -> Either String ReconsTree
getPartialType sigs expr =
  let
    cnvrt : Signature -> SignatureU
    cnvrt (MkSignature name x) = MkSignature name (labelImplicit x)
  in
    getPartialType' (map {f = List} cnvrt sigs) expr