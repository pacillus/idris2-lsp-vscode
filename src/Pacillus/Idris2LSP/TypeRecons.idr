module Pacillus.Idris2LSP.TypeRecons

import Pacillus.Idris2LSP.Syntax.SimpleExpr

import Data.String

%default total

Constraints : Type
Constraints = List (SimpleExpr, SimpleExpr)

-- lowercase and not applied to anything

-- variables from left and variables from right are separated

public export
data PartialExprSignature : Type where
    MkSpSig : (name : SimpleExpr) -> SimpleExpr -> PartialExprSignature

export
toSpSig : Signature -> PartialExprSignature
toSpSig (MkSignature name x) = MkSpSig (IdTerm name) x

getSpSigExpr : PartialExprSignature -> SimpleExpr
getSpSigExpr (MkSpSig name x) = x


public export
data ReconsTree : Type where
    MkStatement : PartialExprSignature -> ReconsTree
    Conclude : List ReconsTree -> PartialExprSignature -> ReconsTree

-- converts to normal signature then show
Show PartialExprSignature where
    show (MkSpSig name x) = show $ MkSignature (MkId $ showSimpleExpr 0 name) x

covering export
Show ReconsTree where
    show (MkStatement sig) = show sig
    show (Conclude xs x) =
      let
        pres = map ((++) "| ") $ foldl (++) [] $ map (lines . show) xs
      in
        unlines $ pres ++ ["----------", show x]


getConclusion : ReconsTree -> PartialExprSignature
getConclusion (MkStatement x) = x
getConclusion (Conclude xs x) = x

mutual
    assignExpr : (target : SimpleExpr)  -> (var : SimpleExpr) -> (replace : SimpleExpr) -> SimpleExpr
    assignExpr target var replace =
      if exEquality target var
        then replace
        else
          case target of
            (IdTerm x) => IdTerm x
            (AppTerm x) => AppTerm $ assignApp x var replace
            (ArwTerm x) => ArwTerm $ assignArw x var replace
            (EqTerm x) => EqTerm $ assignEq x var replace
            (IntegerLiteral k) => IntegerLiteral k
            (DoubleLiteral dbl) => DoubleLiteral dbl
            (StringLiteral str) => StringLiteral str
            (PrTerm x) => PrTerm $ assignPr x var replace
            UnitTerm => UnitTerm

    assignApp : (target : Application) -> (var : SimpleExpr) -> (replace : SimpleExpr) -> Application
    assignApp (MkApp x y) var replace = MkApp (assignExpr x var replace) (assignExpr y var replace)

    assignArw : (target : Arrow b) -> (var : SimpleExpr) -> (replace : SimpleExpr) -> Arrow b
    assignArw (ExExArr x y) var replace = ExExArr (assignExpr x var replace) (assignExpr y var replace)
    assignArw (SiExArr x y) var replace = SiExArr (assignSig x var replace) (assignExpr y var replace)

    assignSig : (target : Signature) -> (var : SimpleExpr) -> (expr : SimpleExpr) -> Signature
    assignSig (MkSignature str x) var expr = MkSignature str $ assignExpr x var expr

    assignEq : (target : Equality) -> (var : SimpleExpr) -> (replace : SimpleExpr) -> Equality
    assignEq (MkEquality x y) var replace = MkEquality (assignExpr x var replace) (assignExpr y var replace)

    assignPr : (target : Pair) -> (var : SimpleExpr) -> (replace : SimpleExpr) -> Pair
    assignPr (MkPair x y) var replace = MkPair (assignExpr x var replace) (assignExpr y var replace)

assignL : (constraints : Constraints) -> (var : SimpleExpr) -> (expr : SimpleExpr) -> Constraints
assignL [] var expr = []
assignL ((x, y) :: xs) var expr = (assignExpr x var expr, y) :: assignL xs var expr

assignR : (constraints : Constraints) -> (var : SimpleExpr) -> (expr : SimpleExpr) -> Constraints
assignR [] var expr = []
assignR ((x, y) :: xs) var expr = (x, assignExpr y var expr) :: assignR xs var expr

isHeadLower : String -> Bool
isHeadLower str =
  case unpack str of
    [] => False
    (x :: xs) => isLower x

isImplicitVar : Identifier -> Bool
isImplicitVar (MkId str) =
  case unpack str of
    [] => False
    (x :: xs) => x == '?'

mutual
    nonImplicitList : SimpleExpr -> List Identifier
    nonImplicitList (IdTerm x) = []
    nonImplicitList (AppTerm x) = nonImplicitListApp x
    nonImplicitList (ArwTerm x) = nonImplicitListArw x
    nonImplicitList (EqTerm x) = nonImplicitListEq x
    nonImplicitList (PrTerm x) = nonImplicitListPr x
    nonImplicitList (IntegerLiteral k) = []
    nonImplicitList (DoubleLiteral dbl) = []
    nonImplicitList (StringLiteral str) = []
    nonImplicitList UnitTerm = []

    nonImplicitListApp : Application -> List Identifier
    nonImplicitListApp (MkApp (IdTerm x) y) = x :: nonImplicitList y
    nonImplicitListApp (MkApp x y) = nonImplicitList x ++ nonImplicitList y

    nonImplicitListArw : Arrow False -> List Identifier
    nonImplicitListArw (ExExArr x y) = nonImplicitList x ++ nonImplicitList y
    nonImplicitListArw (SiExArr (MkSignature name x) y) = name :: nonImplicitList x ++ nonImplicitList y

    nonImplicitListEq : Equality -> List Identifier
    nonImplicitListEq (MkEquality x y) = nonImplicitList x ++ nonImplicitList y

    nonImplicitListPr : Pair -> List Identifier
    nonImplicitListPr (MkPair x y) = nonImplicitList x ++ nonImplicitList y

mutual
    labelImplicit : List Identifier -> SimpleExpr -> SimpleExpr
    labelImplicit nimp (IdTerm (MkId name)) = 
      if isHeadLower name && not ((MkId name) `elem` nimp)
        then IdTerm $ MkId $ "?" ++ name
        else IdTerm $ MkId name
    labelImplicit nimp (AppTerm x) = AppTerm $ labelImplicitApp nimp x
    labelImplicit nimp (ArwTerm x) = ArwTerm $ labelImplicitArw nimp x
    labelImplicit nimp (EqTerm x) = EqTerm $ labelImplicitEq nimp x
    labelImplicit nimp (PrTerm x) = PrTerm $ labelImplicitPr nimp x
    labelImplicit nimp (IntegerLiteral k) = IntegerLiteral k
    labelImplicit nimp (DoubleLiteral dbl) = DoubleLiteral dbl
    labelImplicit nimp (StringLiteral str) = StringLiteral str
    labelImplicit nimp UnitTerm = UnitTerm

    labelImplicitApp : List Identifier -> Application -> Application
    labelImplicitApp nimp (MkApp x y) = MkApp (labelImplicit nimp x) (labelImplicit nimp y)

    labelImplicitArw : List Identifier -> Arrow False -> Arrow False
    labelImplicitArw nimp (ExExArr x y) = ExExArr (labelImplicit nimp x) (labelImplicit nimp y)
    labelImplicitArw nimp (SiExArr (MkSignature str x) y) = SiExArr (MkSignature str (labelImplicit nimp x)) (labelImplicit nimp y)

    labelImplicitEq : List Identifier -> Equality -> Equality
    labelImplicitEq nimp (MkEquality x y) = MkEquality (labelImplicit nimp x) (labelImplicit nimp y)

    labelImplicitPr : List Identifier -> Pair -> Pair
    labelImplicitPr nimp (MkPair x y) = MkPair (labelImplicit nimp x) (labelImplicit nimp y)

export
labelImplicitMain : SimpleExpr -> SimpleExpr
labelImplicitMain x = (labelImplicit . nonImplicitList) x x

mutual
    -- halting is guranteed due to the proof in "Types and Programming Languages" pg.327
    covering
    unify : Constraints -> Either String Constraints
    unify [] = Right []
    unify ((arg, app) :: xs) =
      if exEquality arg app
        then unify xs
        else unifyExpr xs arg app

    showUniError : SimpleExpr -> SimpleExpr -> String
    showUniError x y = "unification failed between " ++ show x ++ " and " ++ show y

    covering
    unifyExpr : (constraints : Constraints) -> (arg : SimpleExpr) -> (applied : SimpleExpr) -> Either String Constraints
    unifyExpr constraints (IdTerm arg) (IdTerm applied) =
      case (isImplicitVar arg, isImplicitVar applied) of
        (True, _) => leafL constraints (IdTerm arg) (IdTerm applied)
        (False, False) =>
          if arg == applied
            then unify constraints
            else Left $ showUniError (IdTerm arg) (IdTerm applied)
        (False, True) => leafR constraints (IdTerm arg) (IdTerm applied)
    unifyExpr constraints (IdTerm arg) applied =
      if isImplicitVar arg
        then leafL constraints (IdTerm arg) applied
        else Left $ showUniError (IdTerm arg) applied
    unifyExpr constraints arg (IdTerm applied) = 
      if isImplicitVar applied
        then leafR constraints arg (IdTerm applied)
        else Left $ showUniError arg (IdTerm applied)
    unifyExpr constraints (AppTerm arg) (AppTerm applied) = unifyApp constraints arg applied
    unifyExpr constraints (ArwTerm arg) (ArwTerm applied) = unifyArw constraints arg applied
    unifyExpr constraints (EqTerm arg) (EqTerm applied) = unifyEq constraints arg applied
    unifyExpr constraints (PrTerm arg) (PrTerm applied) = unifyPr constraints arg applied
    unifyExpr _ arg applied = Left $ showUniError arg applied
    
    
    covering
    unifyApp : (constraints : Constraints) -> (arg : Application) -> (applied : Application) -> Either String Constraints
    unifyApp constraints (MkApp x y) (MkApp z w) = unify $ (x, z) :: (y, w) :: constraints

    covering
    unifyArw : (constraints : Constraints) -> (arg : Arrow False) -> (applied : Arrow False) -> Either String Constraints
    unifyArw constraints (ExExArr x y) (ExExArr z w) = unify $ (x, z) :: (y, w) :: constraints
    unifyArw constraints (ExExArr x y) (SiExArr (MkSignature str z) w) = unify $ (x, z) :: (y, w) :: constraints
    unifyArw constraints (SiExArr (MkSignature str x) y) (ExExArr z w) = unify $ (x, z) :: (y, w) :: constraints
    unifyArw constraints (SiExArr (MkSignature str x) y) (SiExArr (MkSignature str1 z) w) = unify $ (x, z) :: (y, w) :: constraints

    covering
    unifyEq : (constraints : Constraints) -> (arg : Equality) -> (applied : Equality) -> Either String Constraints
    unifyEq constraints (MkEquality x y) (MkEquality z w) = unify $ (x, z) :: (y, w) :: constraints

    covering 
    unifyPr: (constraints : Constraints) -> (arg : Pair) -> (applied : Pair) -> Either String Constraints
    unifyPr constraints (MkPair x y) (MkPair z w) = unify $ (x, z) :: (y, w) :: constraints

    covering
    leafL : (constraints : Constraints) -> (var : SimpleExpr) -> (expr : SimpleExpr) -> Either String Constraints
    leafL constraints var expr =
      do
        unified <- unify $ assignL constraints var expr
        pure $ (var, expr) :: unified

    covering
    leafR : (constraints : Constraints) -> (expr : SimpleExpr) -> (var : SimpleExpr) -> Either String Constraints
    leafR constraints expr var =
      do
        unified <- unify $ assignR constraints var expr
        pure $ (expr, var) :: unified

    

applyConstraints : Constraints -> SimpleExpr -> Either String SimpleExpr
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
getAppliedType (MkSpSig name1 (ArwTerm f)) (MkSpSig name2 x) =
  let
    result_name = MkApp name1 name2
    ExExArr argraw retraw = forgetSig f
    arg = labelImplicitMain argraw
    ret = labelImplicitMain retraw
    x' = labelImplicitMain x
    assign : SimpleExpr -> SimpleExpr
    assign target =
      case f of
        (ExExArr y z) => target
        (SiExArr (MkSignature name y) z) => assignExpr target (IdTerm name) y
  in
  do
    cons <- assert_total $ unify [(arg, x')]
    applied <- applyConstraints cons ret
    Right $ MkSpSig (AppTerm result_name) $ assign applied
getAppliedType _ _ = Left "left side not an appliable form"

-- Right $ MkSignature result_name $ assign applied
-- 






mutual
    export
    getPartialType : (sigs : List Signature) -> SimpleExpr -> Either String ReconsTree
    getPartialType sigs (IdTerm x) = getIdType sigs x
    getPartialType sigs (AppTerm x) = getAppType sigs x
    getPartialType sigs (ArwTerm x) = assert_total $ getArwType sigs x -- in get ArwType value of x always decreases
    getPartialType sigs (EqTerm x) = assert_total $ getEqType sigs x -- totality reasoning is same as aboves
    getPartialType sigs (IntegerLiteral k) = ?getPartialType_rhs_4
    getPartialType sigs (DoubleLiteral dbl) = ?getPartialType_rhs_5
    getPartialType sigs (StringLiteral str) = ?getPartialType_rhs_6
    getPartialType sigs (PrTerm x) = ?getPartialType_rhs_7
    getPartialType sigs UnitTerm =
      let
        sig = MkSpSig (IdTerm $ MkId "MkUnit") UnitTerm
      in
        Right $ MkStatement sig

    getIdType : (sigs : List Signature) -> Identifier -> Either String ReconsTree
    getIdType [] y = Left ""
    getIdType (sig :: sigs) y =
      let
        MkSignature (MkId sig_id) sig_expr = sig
        MkId name = y
      in
      if sig_id == name
        then Right $ MkStatement (toSpSig sig)
        else getIdType sigs y



    getAppType : (sigs : List Signature) -> Application -> Either String ReconsTree
    getAppType sigs (MkApp f x) =
      do
        ftree <- getPartialType sigs f
        xtree <- getPartialType sigs x
        appty <- getAppliedType (getConclusion ftree) (getConclusion xtree)
        pure $ Conclude [ftree, xtree] appty

    getArwType : (sigs : List Signature) -> Arrow False -> Either String ReconsTree
    getArwType sigs x =
      let
        ExExArr argty retty = forgetSig x

        tycnst : SimpleExpr
        tycnst = IdTerm $ MkId "Type"

        isType : PartialExprSignature -> Bool
        isType sig = exEquality (getSpSigExpr sig) tycnst
      in
      do
        argsig <- getPartialType sigs argty
        retsig <- getPartialType sigs retty
        if
            isType (getConclusion argsig)
            &&
            isType (getConclusion retsig)
          then Right (Conclude [argsig, retsig] (MkSpSig (ArwTerm x) tycnst))
          else Left #"Found none "Type" identifier in arrow"#

    getEqType : (sigs : List Signature) -> Equality -> Either String ReconsTree
    getEqType sigs x =
      let
        MkEquality lty rty = x

        tycnst : SimpleExpr
        tycnst = IdTerm $ MkId "Type"

        isSameTy : PartialExprSignature -> PartialExprSignature -> Bool
        isSameTy (MkSpSig _ ty1) (MkSpSig _ ty2) = exEquality ty1 ty2
      in
      do
        lsig <- getPartialType sigs lty
        rsig <- getPartialType sigs rty
        if
            isSameTy (getConclusion lsig) $ getConclusion rsig
          then Right (Conclude [lsig, rsig] (MkSpSig (EqTerm x) tycnst))
          else Left #"Found none "Type" identifier in arrow"#