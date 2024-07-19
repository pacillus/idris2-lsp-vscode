module Pacillus.Idris2LSP.TypeRecons.TypeRecons

import Data.String
import Text.Parser
import Text.Parser.Expression

import Pacillus.Idris2LSP.Syntax.SimpleExpr
import Pacillus.Idris2LSP.Syntax.Lexer

%default total

SimpleExprU : Type
SimpleExprU = SimpleExprProt (Bool, String)

IdentifierU : Type
IdentifierU = IdentifierProt (Bool, String)

ApplicationU : Type
ApplicationU = ApplicationProt (Bool, String)

EqualityU : Type
EqualityU = EqualityProt (Bool, String)

ArrowU : Bool -> Type
ArrowU = ArrowProt (Bool, String)

PairU :Type
PairU = PairProt (Bool, String)

SignatureU : Type
SignatureU = SignatureProt (Bool, String)

Constraints : Type
Constraints = List (SimpleExprU, SimpleExprU)

Eq IdentifierU where
  (MkId (x, z)) == (MkId (y, w)) = x == y && nameeq z w

Show IdentifierU where
  show (MkId (b, str)) = ifThenElse b "?" "" ++ str

toTypeVar : Identifier -> IdentifierU
toTypeVar (MkId x) = MkId (True, x)


allAsConstant : SimpleExpr -> SimpleExprU
allAsConstant (IdTerm (MkId x)) = IdTerm (MkId (False, x))
allAsConstant (AppTerm (MkApp x y)) = AppTerm (MkApp (allAsConstant x) (allAsConstant y))
allAsConstant (ArwTerm (ExExArr x y)) = ArwTerm (ExExArr (allAsConstant x) (allAsConstant y))
allAsConstant (ArwTerm (SiExArr (MkSignature name x) y)) = 
  ArwTerm $ SiExArr (MkSignature name (allAsConstant x)) (allAsConstant y)
allAsConstant (EqTerm (MkEquality x y)) = EqTerm $ MkEquality (allAsConstant x) (allAsConstant y)
allAsConstant (IntegerLiteral i) = IntegerLiteral i
allAsConstant (DoubleLiteral dbl) = DoubleLiteral dbl
allAsConstant (CharLiteral c) = CharLiteral c
allAsConstant (StringLiteral str) = StringLiteral str
allAsConstant (PrTerm (MkPair x y)) = PrTerm $ MkPair (allAsConstant x) (allAsConstant y)
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
            (EqTerm x) => EqTerm $ assignEq x var replace
            (IntegerLiteral k) => IntegerLiteral k
            (DoubleLiteral dbl) => DoubleLiteral dbl
            (CharLiteral c) => CharLiteral c
            (StringLiteral str) => StringLiteral str
            (PrTerm x) => PrTerm $ assignPr x var replace
            UnitTerm => UnitTerm

    assignApp : (target : ApplicationU) -> (var : SimpleExprU) -> (replace : SimpleExprU) -> ApplicationU
    assignApp (MkApp x y) var replace = MkApp (assignExpr x var replace) (assignExpr y var replace)

    assignArw : (target : ArrowU b) -> (var : SimpleExprU) -> (replace : SimpleExprU) -> ArrowU b
    assignArw (ExExArr x y) var replace = ExExArr (assignExpr x var replace) (assignExpr y var replace)
    assignArw (SiExArr x y) var replace = SiExArr (assignSig x var replace) (assignExpr y var replace)

    assignSig : (target : SignatureU) -> (var : SimpleExprU) -> (expr : SimpleExprU) -> SignatureU
    assignSig (MkSignature str x) var expr = MkSignature str $ assignExpr x var expr

    assignEq : (target : EqualityU) -> (var : SimpleExprU) -> (replace : SimpleExprU) -> EqualityU
    assignEq (MkEquality x y) var replace = MkEquality (assignExpr x var replace) (assignExpr y var replace)

    assignPr : (target : PairU) -> (var : SimpleExprU) -> (replace : SimpleExprU) -> PairU
    assignPr (MkPair x y) var replace = MkPair (assignExpr x var replace) (assignExpr y var replace)

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
isImplicitVar (MkId (imp, str)) = imp

mutual
    labelImplicitExp : List Identifier -> SimpleExpr -> SimpleExprU
    labelImplicitExp nimp (IdTerm (MkId name)) = 
      if isHeadLower name && not ((MkId name) `elem` nimp)
        then IdTerm $ MkId $ (True, name)
        else IdTerm $ MkId (False, name)
    labelImplicitExp nimp (AppTerm x) = AppTerm $ labelImplicitApp nimp x -- TODO if left is a var it might be defined
    labelImplicitExp nimp (ArwTerm x) = ArwTerm $ labelImplicitArw nimp x
    labelImplicitExp nimp (EqTerm x) = EqTerm $ labelImplicitEq nimp x
    labelImplicitExp nimp (PrTerm x) = PrTerm $ labelImplicitPr nimp x
    labelImplicitExp nimp (IntegerLiteral k) = IntegerLiteral k
    labelImplicitExp nimp (DoubleLiteral dbl) = DoubleLiteral dbl
    labelImplicitExp nimp (CharLiteral c) = CharLiteral c
    labelImplicitExp nimp (StringLiteral str) = StringLiteral str
    labelImplicitExp nimp UnitTerm = UnitTerm

    labelImplicitApp : List Identifier -> Application -> ApplicationU
    labelImplicitApp nimp (MkApp (IdTerm (MkId x)) y) = MkApp (IdTerm $ MkId (False, x)) (labelImplicitExp nimp y) -- applying identifier is not a variable
    labelImplicitApp nimp (MkApp x y) = MkApp (labelImplicitExp nimp x) (labelImplicitExp nimp y)

    labelImplicitArw : List Identifier -> Arrow False -> ArrowU False
    labelImplicitArw nimp (ExExArr x y) = ExExArr (labelImplicitExp nimp x) (labelImplicitExp nimp y)
    labelImplicitArw nimp (SiExArr (MkSignature str x) y) = SiExArr (MkSignature str (labelImplicitExp nimp x)) (labelImplicitExp nimp y)

    labelImplicitEq : List Identifier -> Equality -> EqualityU
    labelImplicitEq nimp (MkEquality x y) = MkEquality (labelImplicitExp nimp x) (labelImplicitExp nimp y)

    labelImplicitPr : List Identifier -> Pair -> PairU
    labelImplicitPr nimp (MkPair x y) = MkPair (labelImplicitExp nimp x) (labelImplicitExp nimp y)


labelImplicitMain : List Identifier -> SimpleExpr -> SimpleExprU
labelImplicitMain nimp (ArwTerm (ExExArr x y)) = ArwTerm $ ExExArr (labelImplicitExp nimp x) (labelImplicitMain nimp y)
labelImplicitMain nimp (ArwTerm (SiExArr (MkSignature name x) y)) = ArwTerm $ SiExArr (MkSignature name $ labelImplicitExp nimp x) $ labelImplicitMain (name :: nimp) y
labelImplicitMain nimp x = labelImplicitExp nimp x

export
labelImplicit : SimpleExpr -> SimpleExprU
labelImplicit x = labelImplicitMain [] x

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
    unifyExpr constraints (IdTerm arg) (IdTerm applied) =
      case (isImplicitVar arg, isImplicitVar applied) of
        (True, _) => uniVarExprL constraints (IdTerm arg) (IdTerm applied)
        (False, False) =>
          if arg == applied
            then unify constraints
            else Left $ showUniError (IdTerm arg) (IdTerm applied)
        (False, True) => uniVarExprR constraints (IdTerm arg) (IdTerm applied)
    unifyExpr constraints (IdTerm arg) applied =
      if isImplicitVar arg
        then uniVarExprL constraints (IdTerm arg) applied
        else Left $ showUniError (IdTerm arg) applied
    unifyExpr constraints arg (IdTerm applied) = 
      if isImplicitVar applied
        then uniVarExprR constraints arg (IdTerm applied)
        else Left $ showUniError arg (IdTerm applied)
    unifyExpr constraints (AppTerm arg) (AppTerm applied) = unifyApp constraints arg applied
    unifyExpr constraints (ArwTerm arg) (ArwTerm applied) = unifyArw constraints arg applied
    unifyExpr constraints (EqTerm arg) (EqTerm applied) = unifyEq constraints arg applied
    unifyExpr constraints (PrTerm arg) (PrTerm applied) = unifyPr constraints arg applied
    unifyExpr _ arg applied = Left $ showUniError arg applied -- error
    
    
    covering
    unifyApp : (constraints : Constraints) -> (arg : ApplicationU) -> (applied : ApplicationU) -> Either String Constraints
    unifyApp constraints (MkApp x y) (MkApp z w) = unify $ (x, z) :: (y, w) :: constraints

    covering
    unifyArw : (constraints : Constraints) -> (arg : ArrowU False) -> (applied : ArrowU False) -> Either String Constraints
    unifyArw constraints (ExExArr x y) (ExExArr z w) = unify $ (x, z) :: (y, w) :: constraints
    unifyArw constraints (ExExArr x y) (SiExArr (MkSignature str z) w) = unify $ (x, z) :: (y, w) :: constraints
    unifyArw constraints (SiExArr (MkSignature str x) y) (ExExArr z w) = unify $ (x, z) :: (y, w) :: constraints
    unifyArw constraints (SiExArr (MkSignature str x) y) (SiExArr (MkSignature str1 z) w) = unify $ (x, z) :: (y, w) :: constraints

    covering
    unifyEq : (constraints : Constraints) -> (arg : EqualityU) -> (applied : EqualityU) -> Either String Constraints
    unifyEq constraints (MkEquality x y) (MkEquality z w) = unify $ (x, z) :: (y, w) :: constraints

    covering 
    unifyPr: (constraints : Constraints) -> (arg : PairU) -> (applied : PairU) -> Either String Constraints
    unifyPr constraints (MkPair x y) (MkPair z w) = unify $ (x, z) :: (y, w) :: constraints

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
    getPartialType' sigs (EqTerm x) = getEqType sigs x
    getPartialType' sigs (IntegerLiteral k) = Right $ Start $ MkSpSig (IdTerm $ MkId $ show k) $ IdTerm $ MkId (False, "Integer")
    getPartialType' sigs (DoubleLiteral dbl) = Right $ Start $ MkSpSig (IdTerm $ MkId $ show dbl) $ IdTerm $ MkId (False, "Double")
    getPartialType' sigs (CharLiteral c) = Right $ Start $ MkSpSig (IdTerm $ MkId $ show c) $ IdTerm $ MkId (False, "Char")
    getPartialType' sigs (StringLiteral str) = Right $ Start $ MkSpSig (IdTerm $ MkId $ show str) $ IdTerm $ MkId (False, "String")
    getPartialType' sigs (PrTerm x) = getPrType sigs x
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



    getAppType : (sigs : List SignatureU) -> Application -> Either String ReconsTree
    getAppType sigs (MkApp f x) =
      do
        ftree <- getPartialType' sigs f
        xtree <- getPartialType' sigs x
        appty <- getAppliedType (getSubgoal ftree) (getSubgoal xtree)
        pure $ Subgoal [ftree, xtree] appty

    getArwType : (sigs : List SignatureU) -> Arrow False -> Either String ReconsTree
    getArwType sigs x with (forgetSig x)
      getArwType sigs x | ExExArr argty retty = 
          let
            tycnst : SimpleExprU
            tycnst = IdTerm $ MkId (False, "Type")

            isType : PartialExprSignature -> Bool
            isType sig = exprEquality (getSpSigExpr sig) tycnst
          in
          do
            argsig <- getPartialType' sigs argty
            retsig <- getPartialType' sigs retty
            if isType (getSubgoal argsig) && isType (getSubgoal retsig)
                then Right (Subgoal [argsig, retsig] (MkSpSig (ArwTerm x) tycnst))
                else Left #"Found none "Type" identifier in arrow"#

    getEqType : (sigs : List SignatureU) -> Equality -> Either String ReconsTree
    getEqType sigs (MkEquality lty rty) =
      let
        tycnst : SimpleExprU
        tycnst = IdTerm $ MkId (False, "Type")

        isSameTy : PartialExprSignature -> PartialExprSignature -> Bool
        isSameTy (MkSpSig _ ty1) (MkSpSig _ ty2) = exprEquality ty1 ty2
      in
      do
        lsig <- getPartialType' sigs lty
        rsig <- getPartialType' sigs rty
        Right (Subgoal [lsig, rsig] (MkSpSig (EqTerm $ MkEquality lty rty) tycnst))
         

    getPrType : (sigs : List SignatureU) -> Pair -> Either String ReconsTree
    getPrType sigs (MkPair x y) =
      let
        mkpair : ReconsTree -> ReconsTree -> SimpleExprU
        mkpair x y = PrTerm $ MkPair (getSpSigExpr $ getSubgoal x) (getSpSigExpr $ getSubgoal y)
      in
      do
        xsig <- getPartialType' sigs x
        ysig <- getPartialType' sigs y
        Right (Subgoal [xsig, ysig] (MkSpSig (PrTerm $ MkPair x y) $ mkpair xsig ysig))

    getIntLitType : (sigs : List SignatureU) -> Integer -> Either String ReconsTree
    getIntLitType sigs k = ?rhs

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