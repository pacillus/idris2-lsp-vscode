module Pacillus.Idris2LSP.Parser.Desugared

import Data.List

import Pacillus.Idris2LSP.Syntax.Basic

operatorToIdentifier : Operator -> Identifier
operatorToIdentifier (MkOperator str) = MkIdentifier str

MemberToIdentifier : Member -> Identifier
MemberToIdentifier (MkMember str) = MkIdentifier str


indexMain : Nat -> List BinderName -> Identifier -> Maybe Nat 
indexMain _ [] _ = Nothing
indexMain k (NamedBinder x :: xs) y with (x == y)
  indexMain k (NamedBinder x :: xs) y | False = indexMain (S k) xs y
  indexMain k (NamedBinder x :: xs) y | True = Just k
indexMain k (AnonymousBinder :: xs) y = indexMain (S k) xs y

index : List BinderName -> Identifier -> Maybe Nat
index xs x = indexMain 0 xs x




-- List BinderName is the list of binder-identifiers from inside
--   it is used to index the variables
export
desugarWithContext : List BinderName -> Sugared Expr -> List (Desugared NoHole)
desugarWithContext xs (IdentifierTerm x) with (index xs x)
  desugarWithContext xs (IdentifierTerm x) | Nothing = [Constant NameId x]
  desugarWithContext xs (IdentifierTerm x) | Just y = [Index y]
desugarWithContext xs (Application f x) = 
  do
    f' <- desugarWithContext xs f
    x' <- desugarWithContext xs x
    [Application f' x']
desugarWithContext xs (Arrow SingleLine ty e) = 
  do
    ty' <- desugarWithContext xs ty 
    e' <- desugarWithContext (AnonymousBinder :: xs) e
    [Binder Pi AnonymousBinder ty' e']
desugarWithContext xs (Arrow DoubleLine ty e) =
  do
    ty' <- desugarWithContext xs ty
    e' <- desugarWithContext (AnonymousBinder :: xs) e
    [Binder Auto AnonymousBinder ty' e']
desugarWithContext xs (SignatureArrow SingleLine (Signature id ty) e) = 
  do
    ty' <- desugarWithContext xs ty
    e' <- desugarWithContext (NamedBinder id :: xs) e
    [Binder Pi (NamedBinder id) ty' e']
desugarWithContext xs (SignatureArrow DoubleLine (Signature id ty) e) = 
  do
    ty' <- desugarWithContext xs ty 
    e' <- desugarWithContext (NamedBinder id :: xs) e
    [Binder Auto (NamedBinder id) ty' e']
desugarWithContext xs (BracketArrow (Signature id ty) e) = 
  do
    ty' <- desugarWithContext xs ty
    e' <- desugarWithContext (NamedBinder id :: xs) e
    [Binder Implicit (NamedBinder id) ty' e']
desugarWithContext xs (AnonymousFunction id e) = 
  do 
    e' <- desugarWithContext (NamedBinder id :: xs) e
    [Binder Lambda (NamedBinder id) WildCard e']
desugarWithContext xs (Literal a x) = [Literal a x]
desugarWithContext xs UnitSugar = [Constant NameId $ MkIdentifier "Unit", Constant NameId $ MkIdentifier "MkUnit"]
desugarWithContext xs (PairSugar x y) =
  do
    x' <- desugarWithContext xs x 
    y' <- desugarWithContext xs y
    [Application  x' y']
desugarWithContext xs (OpInfixSugar x op y) = 
  do
    x' <- desugarWithContext xs x 
    y' <- desugarWithContext xs y
    [Application (Application (Constant OperatorId $ operatorToIdentifier op) x') y']
desugarWithContext xs (InfixSugar x fun y) =
  do
    x' <- desugarWithContext xs x 
    y' <- desugarWithContext xs y
    [Application (Application (Constant InfixId fun) x') y']
desugarWithContext xs (DependentPairSugar id a b) = 
  let
    dpair : Desugared NoHole
    dpair = Constant NameId $ MkIdentifier "DPair"
  in
      do
        a' <- desugarWithContext xs a
        b' <- desugarWithContext (NamedBinder id :: xs) b
        [Application (Application dpair a') (Binder Lambda (NamedBinder id) WildCard b')]
desugarWithContext xs (EqualSugar e1 e2) = 
  let
    equal1 : Desugared NoHole
    equal1 = Constant OperatorId $ MkIdentifier "==="
    equal2 : Desugared NoHole
    equal2 = Constant OperatorId $ MkIdentifier "~=~"
  in
      do
        eqs <- [equal1, equal2]
        e1' <- desugarWithContext xs e1
        e2' <- desugarWithContext xs e2
        [Application (Application eqs e1') e2']
desugarWithContext xs (MemberSugar x f) = 
  do
    x' <- desugarWithContext xs x
    [Application (Constant MemberId $ MemberToIdentifier f) x']
desugarWithContext xs (DollarSugar f x) = 
  do
    f' <- desugarWithContext xs f
    x' <- desugarWithContext xs x
    [Application f' x']

-- === the main function ===
export
desugar : Sugared Expr -> List (Desugared NoHole)
desugar x = desugarWithContext [] x

export
desugarSig : Sugared Sig -> List (DesugaredSignature NoHole)
desugarSig (Signature x y) =
  do
    y' <- desugar y
    [MkDSig NameId x y']
-- === sub functions ===

getImplicitList : Desugared NoHole -> List Identifier
getImplicitList (Constant NameId x@(MkIdentifier str)) =
  let
    isHeadLower : String -> Bool
    isHeadLower str =
      case unpack str of
        [] => False
        (x :: xs) => isLower x
  in
  if isHeadLower str
    then [x]
    else []
getImplicitList (Constant _ _) = []
getImplicitList (Index _) = []
getImplicitList (Application (Constant _ _) x) = getImplicitList x
getImplicitList (Application f x) = getImplicitList f ++ getImplicitList x
getImplicitList (Binder _ (NamedBinder id) ty e) = getImplicitList ty ++ getImplicitList e --no need to delete id from "getImplicitList" since they are already deleted by desugaring
getImplicitList (Binder _ AnonymousBinder ty e) = getImplicitList ty ++ getImplicitList e
getImplicitList (Literal _ _) = []
getImplicitList WildCard = []

addImplictsFromList : List Identifier -> Desugared NoHole -> Desugared NoHole
addImplictsFromList [] e = e
addImplictsFromList (id :: ids) e = addImplictsFromList ids $ Binder Implicit (NamedBinder id) WildCard e

addImplicits : Desugared NoHole -> Desugared NoHole
addImplicits e = addImplictsFromList (getImplicitList e) e

export
desugarType : Sugared Expr -> List (Desugared NoHole)
desugarType e =
  do
   e' <- desugar e
   [addImplicits e']
