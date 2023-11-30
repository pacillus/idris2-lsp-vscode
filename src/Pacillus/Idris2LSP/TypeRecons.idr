module Pacillus.Idris2LSP.TypeRecons

import Data.String
import Text.Parser
import Text.Parser.Expression

import Pacillus.Idris2LSP.Syntax.SimpleExpr
import Pacillus.Idris2LSP.Syntax.Lexer

%default total

Constraints : Type
Constraints = List (SimpleExpr, SimpleExpr)

parseSignature : List (WithBounds SimpleExprToken) -> Either String Signature
parseSignature toks =
  case parse (signature opTable) $ filter (not . ignored) toks of
    Right (l, []) => Right l
    Right (l, xs) => Left "contains tokens that were not consumed"
    Left e => Left (show e)

export
parseSig : String -> Either String Signature
parseSig x with (lexSimpleExpr x)
  parseSig x | Just toks = parseSignature toks
  parseSig x | Nothing = Left "Failed to lex."

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

-- mutual
--     nonImplicitList : SimpleExpr -> List Identifier
--     nonImplicitList (IdTerm x) = []
--     nonImplicitList (AppTerm x) = nonImplicitListApp x
--     nonImplicitList (ArwTerm x) = nonImplicitListArw x
--     nonImplicitList (EqTerm x) = nonImplicitListEq x
--     nonImplicitList (PrTerm x) = nonImplicitListPr x
--     nonImplicitList (IntegerLiteral k) = []
--     nonImplicitList (DoubleLiteral dbl) = []
--     nonImplicitList (StringLiteral str) = []
--     nonImplicitList UnitTerm = []

--     nonImplicitListApp : Application -> List Identifier
--     nonImplicitListApp (MkApp (IdTerm x) y) = x :: nonImplicitList y
--     nonImplicitListApp (MkApp x y) = nonImplicitList x ++ nonImplicitList y

--     nonImplicitListArw : Arrow False -> List Identifier
--     nonImplicitListArw (ExExArr x y) = nonImplicitList x ++ nonImplicitList y
--     nonImplicitListArw (SiExArr (MkSignature name x) y) = name :: nonImplicitList x ++ nonImplicitList y

--     nonImplicitListEq : Equality -> List Identifier
--     nonImplicitListEq (MkEquality x y) = nonImplicitList x ++ nonImplicitList y

--     nonImplicitListPr : Pair -> List Identifier
--     nonImplicitListPr (MkPair x y) = nonImplicitList x ++ nonImplicitList y

mutual
    labelmplicitExp : List Identifier -> SimpleExpr -> SimpleExpr
    labelmplicitExp nimp (IdTerm (MkId name)) = 
      if isHeadLower name && not ((MkId name) `elem` nimp)
        then IdTerm $ MkId $ "?" ++ name
        else IdTerm $ MkId name
    labelmplicitExp nimp (AppTerm x) = AppTerm $ labelImplicitApp nimp x
    labelmplicitExp nimp (ArwTerm x) = ArwTerm $ labelImplicitArw nimp x
    labelmplicitExp nimp (EqTerm x) = EqTerm $ labelImplicitEq nimp x
    labelmplicitExp nimp (PrTerm x) = PrTerm $ labelImplicitPr nimp x
    labelmplicitExp nimp (IntegerLiteral k) = IntegerLiteral k
    labelmplicitExp nimp (DoubleLiteral dbl) = DoubleLiteral dbl
    labelmplicitExp nimp (StringLiteral str) = StringLiteral str
    labelmplicitExp nimp UnitTerm = UnitTerm

    labelImplicitApp : List Identifier -> Application -> Application
    labelImplicitApp nimp (MkApp x y) = MkApp (labelmplicitExp nimp x) (labelmplicitExp nimp y)

    labelImplicitArw : List Identifier -> Arrow False -> Arrow False
    labelImplicitArw nimp (ExExArr x y) = ExExArr (labelmplicitExp nimp x) (labelmplicitExp nimp y)
    labelImplicitArw nimp (SiExArr (MkSignature str x) y) = SiExArr (MkSignature str (labelmplicitExp nimp x)) (labelmplicitExp nimp y)

    labelImplicitEq : List Identifier -> Equality -> Equality
    labelImplicitEq nimp (MkEquality x y) = MkEquality (labelmplicitExp nimp x) (labelmplicitExp nimp y)

    labelImplicitPr : List Identifier -> Pair -> Pair
    labelImplicitPr nimp (MkPair x y) = MkPair (labelmplicitExp nimp x) (labelmplicitExp nimp y)


labelImplicitMain : List Identifier -> SimpleExpr -> SimpleExpr
labelImplicitMain nimp (ArwTerm (ExExArr x y)) = ArwTerm $ ExExArr (labelmplicitExp nimp x) (labelImplicitMain nimp y)
labelImplicitMain nimp (ArwTerm (SiExArr (MkSignature name x) y)) = ArwTerm $ SiExArr (MkSignature name $ labelmplicitExp nimp x) $ labelImplicitMain (name :: nimp) y
labelImplicitMain nimp x = labelmplicitExp nimp x

export
labelImplicit : SimpleExpr -> SimpleExpr
labelImplicit x = labelImplicitMain [] x

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
getAppliedType (MkSpSig name1 f) x with (labelImplicit f)
    getAppliedType (MkSpSig name1 f) (MkSpSig name2 xraw) | (ArwTerm f') =
      let
        result_name = MkApp name1 name2
        ExExArr arg ret = forgetSig f'
        x = labelImplicit xraw
        assign : (target : SimpleExpr) -> (to : SimpleExpr) -> SimpleExpr
        assign target to =
        case f' of
            (ExExArr y z) => target
            (SiExArr (MkSignature name y) z) => assignExpr target (IdTerm name) to
      in
      do
        cons <- assert_total $ unify [(arg, x)]
        applied <- applyConstraints cons ret
        Right $ MkSpSig (AppTerm result_name) $ assign applied name2
    getAppliedType (MkSpSig name1 (ArwTerm f)) (MkSpSig name2 xraw) | _ = Left "unexpected error: arrow changed by labeling implicit"
    getAppliedType (MkSpSig name1 f) _ | _ = Left "left side not an appliable form"

-- Right $ MkSignature result_name $ assign applied
-- 





-- TODO after implementing unification for interface, add from[LiteralType] to all literals
mutual
    export
    getPartialType : (sigs : List Signature) -> SimpleExpr -> Either String ReconsTree
    getPartialType sigs (IdTerm x) = getIdType sigs x
    getPartialType sigs (AppTerm x) = getAppType sigs x
    getPartialType sigs (ArwTerm x) = assert_total $ getArwType sigs x -- in getArwType, value of x always decreases
    getPartialType sigs (EqTerm x) = getEqType sigs x
    getPartialType sigs (IntegerLiteral k) = Right $ MkStatement $ MkSpSig (IdTerm $ MkId $ show k) $ IdTerm $ MkId "Integer"
    getPartialType sigs (DoubleLiteral dbl) = Right $ MkStatement $ MkSpSig (IdTerm $ MkId $ show dbl) $ IdTerm $ MkId "Double"
    getPartialType sigs (StringLiteral str) = Right $ MkStatement $ MkSpSig (IdTerm $ MkId $ show str) $ IdTerm $ MkId "String"
    getPartialType sigs (PrTerm x) = getPrType sigs x
    getPartialType sigs UnitTerm =
      let
        sig = MkSpSig (IdTerm $ MkId "MkUnit") UnitTerm
      in
        Right $ MkStatement sig

    getIdType : (sigs : List Signature) -> Identifier -> Either String ReconsTree
    getIdType [] y = Left $ "could not find the type of identifier " ++ show y
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
    getArwType sigs x with (forgetSig x)
      getArwType sigs x | ExExArr argty retty = 
          let
            tycnst : SimpleExpr
            tycnst = IdTerm $ MkId "Type"

            isType : PartialExprSignature -> Bool
            isType sig = exEquality (getSpSigExpr sig) tycnst
          in
          do
            argsig <- getPartialType sigs argty
            retsig <- getPartialType sigs retty
            if isType (getConclusion argsig) && isType (getConclusion retsig)
                then Right (Conclude [argsig, retsig] (MkSpSig (ArwTerm x) tycnst))
                else Left #"Found none "Type" identifier in arrow"#

    getEqType : (sigs : List Signature) -> Equality -> Either String ReconsTree
    getEqType sigs (MkEquality lty rty) =
      let
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
          then Right (Conclude [lsig, rsig] (MkSpSig (EqTerm $ MkEquality lty rty) tycnst))
          else Left #"Found none "Type" identifier in arrow"#

    getPrType : (sigs : List Signature) -> Pair -> Either String ReconsTree
    getPrType sigs (MkPair x y) =
      let
        mkpair : ReconsTree -> ReconsTree -> SimpleExpr
        mkpair x y = PrTerm $ MkPair (getSpSigExpr $ getConclusion x) (getSpSigExpr $ getConclusion y)
      in
      do
        xsig <- getPartialType sigs x
        ysig <- getPartialType sigs y
        Right (Conclude [xsig, ysig] (MkSpSig (PrTerm $ MkPair x y) $ mkpair xsig ysig))

    getIntLitType : (sigs : List Signature) -> Integer -> Either String ReconsTree
    getIntLitType sigs k = ?rhs

    -- getLitType : (sigs : List Signature) -> (fromlitty : PartialExprSignature) -> (literal : SimpleExpr) -> Either String ReconsTree
    -- getLitType sigs fromlitty literal = ?getLitType_rhs

    