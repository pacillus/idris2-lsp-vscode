module Pacillus.Idris2LSP.TypeRecons

import Pacillus.Idris2LSP.Syntax.SimpleExpr

%default total

Constraints : Type
Constraints = List (SimpleExpr, SimpleExpr)


-- lowercase and not applied to anything

-- variables from left and variables from right are separated

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
            (NatLiteral k) => NatLiteral k
            (DoubleLiteral dbl) => DoubleLiteral dbl
            (StringLiteral str) => StringLiteral str
            (PrTerm x) => PrTerm $ assignPr x var replace
            UnitTerm => ?rjs_8

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
    nonImplicitList (NatLiteral k) = []
    nonImplicitList (DoubleLiteral dbl) = []
    nonImplicitList (StringLiteral str) = []
    nonImplicitList UnitTerm = []

    nonImplicitListApp : Application -> List Identifier
    nonImplicitListApp (MkApp (IdTerm x) y) = x :: nonImplicitList y
    nonImplicitListApp (MkApp x y) = nonImplicitList x ++ nonImplicitList y

    nonImplicitListArw : Arrow False -> List Identifier
    nonImplicitListArw (ExExArr x y) = nonImplicitList x ++ nonImplicitList y
    nonImplicitListArw (SiExArr (MkSignature name x) y) = MkId name :: nonImplicitList x ++ nonImplicitList y

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
    labelImplicit nimp (NatLiteral k) = NatLiteral k
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

    covering
    unifyExpr : (constraints : Constraints) -> (arg : SimpleExpr) -> (applied : SimpleExpr) -> Either String Constraints
    unifyExpr constraints (IdTerm arg) applied =
      if isImplicitVar arg
        then leafL constraints (IdTerm arg) applied
        else Left "unification failed"
    unifyExpr constraints arg (IdTerm applied) = 
      if isImplicitVar applied
        then leafR constraints arg (IdTerm applied)
        else Left "unification failed"
    unifyExpr constraints (IdTerm arg) (IdTerm applied) =
      if arg == applied
        then unify constraints
        else Left "unification failed"
    unifyExpr constraints (AppTerm arg) (AppTerm applied) = unifyApp constraints arg applied
    unifyExpr constraints (ArwTerm arg) (ArwTerm applied) = unifyArw constraints arg applied
    unifyExpr constraints (EqTerm arg) (EqTerm applied) = unifyEq constraints arg applied
    unifyExpr constraints (PrTerm arg) (PrTerm applied) = unifyPr constraints arg applied
    unifyExpr _ _ _ = Left "unification failed"
    
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
getAppliedType : (f : Signature) -> (x : Signature) -> Either String Signature
getAppliedType (MkSignature str1 (ArwTerm f)) (MkSignature str2 x) =
  let
    result_name = str1 ++ " " ++ str2
    ExExArr argraw retraw = forgetSig f
    arg = labelImplicitMain argraw
    ret = labelImplicitMain retraw
    assign : SimpleExpr -> SimpleExpr
    assign target =
      case f of
        (ExExArr y z) => target
        (SiExArr (MkSignature str y) z) => assignExpr target (IdTerm $ MkId str) y
  in
  do
    cons <- assert_total $ unify [(arg, x)]
    applied <- applyConstraints cons ret
    Right $ MkSignature result_name $ assign applied
getAppliedType _ _ = Left "left side not an appliable form"

-- Right $ MkSignature result_name $ assign applied
--