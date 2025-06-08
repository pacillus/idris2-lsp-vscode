module Pacillus.Idris2LSP.Parser.Desugared

import Data.List
import Data.List1

import Pacillus.Idris2LSP.Parser.Basic

operatorToIdentifier : Operator -> Identifier
operatorToIdentifier (MkOperator str) = MkIdentifier OperatorId str

MemberToIdentifier : Member -> Identifier
MemberToIdentifier (MkMember str) = MkIdentifier MemberId str

indexMain : Nat -> List BinderName -> Identifier -> Maybe Nat 
indexMain _ [] _ = Nothing
indexMain k (NamedBinder x :: xs) y with (x == y)
  indexMain k (NamedBinder x :: xs) y | False = indexMain (S k) xs y
  indexMain k (NamedBinder x :: xs) y | True = Just k
indexMain k (AnonymousBinder :: xs) y = indexMain (S k) xs y

index : List BinderName -> Identifier -> Maybe Nat
index xs x = indexMain 0 xs x

-- List BinderName is the list of binder-identifiers from inside
-- They are used to index the variables
export
desugarWithContext : List BinderName -> Sugared Expr -> List1 (Desugared NoHole)
desugarWithContext xs (IdentifierTerm x) with (index xs x)
  desugarWithContext xs (IdentifierTerm x) | Nothing = Constant x ::: []
  desugarWithContext xs (IdentifierTerm x) | Just y = Index x y ::: []
desugarWithContext xs (Application f x) =
  [ 
    Application f' x' |
    f' <- desugarWithContext xs f,
    x' <- desugarWithContext xs x
  ]
desugarWithContext xs (Arrow SingleLine ty e) = 
  [
    Binder Pi AnonymousBinder ty' e' |
    ty' <- desugarWithContext xs ty,
    e' <- desugarWithContext (AnonymousBinder :: xs) e
  ]
desugarWithContext xs (Arrow DoubleLine ty e) =
  [
    Binder Auto AnonymousBinder ty' e' |
    ty' <- desugarWithContext xs ty,
    e' <- desugarWithContext (AnonymousBinder :: xs) e
  ]
desugarWithContext xs (SignatureArrow SingleLine (Signature id ty) e) = 
  [
    Binder Pi (NamedBinder id) ty' e' |
    ty' <- desugarWithContext xs ty,
    e' <- desugarWithContext (NamedBinder id :: xs) e
  ]
desugarWithContext xs (SignatureArrow DoubleLine (Signature id ty) e) = 
  [
    Binder Auto (NamedBinder id) ty' e' |
    ty' <- desugarWithContext xs ty,
    e' <- desugarWithContext (NamedBinder id :: xs) e
  ]
desugarWithContext xs (BracketArrow (Signature id ty) e) = 
  [
    Binder Implicit (NamedBinder id) ty' e' |
    ty' <- desugarWithContext xs ty,
    e' <- desugarWithContext (NamedBinder id :: xs) e
  ]
desugarWithContext xs (AnonymousFunction id e) = 
  [
    Binder Lambda (NamedBinder id) Wildcard e' |
    e' <- desugarWithContext (NamedBinder id :: xs) e
  ]
desugarWithContext xs (Literal IntegerL x) = pure $ Application (Constant (MkIdentifier NameId "fromInteger")) (Literal IntegerL x)
desugarWithContext xs (Literal DoubleL x) = pure $ Application (Constant (MkIdentifier NameId "fromDouble")) (Literal DoubleL x)
desugarWithContext xs (Literal CharL x) = pure $ Application (Constant (MkIdentifier NameId "fromChar")) (Literal CharL x)
desugarWithContext xs (Literal StringL x) = pure $ Application (Constant (MkIdentifier NameId "fromString")) (Literal StringL x) -- pure $ Literal a x
desugarWithContext xs Wildcard = pure $ Wildcard
desugarWithContext xs UnitSugar = (Constant $ MkIdentifier NameId "Unit") ::: [Constant $ MkIdentifier NameId "MkUnit"]
desugarWithContext xs (PairSugar x y) =
  [
    Application (Application (Constant $ MkIdentifier NameId candidate) x') y' |
    x' <- desugarWithContext xs x,
    y' <- desugarWithContext xs y,
    candidate <- "Pair" ::: ["MkPair"]
  ]
desugarWithContext xs (OpInfixSugar x op y) = 
  [
    Application (Application (Constant $ operatorToIdentifier op) x') y' |
    x' <- desugarWithContext xs x,
    y' <- desugarWithContext xs y
  ]
desugarWithContext xs (InfixSugar x fun y) =
  [
    Application (Application (Constant fun) x') y' |
    x' <- desugarWithContext xs x,
    y' <- desugarWithContext xs y
  ]
desugarWithContext xs (DependentPairSugar id a b) = 
  let
    dpair : Desugared NoHole
    dpair = Constant $ MkIdentifier NameId "DPair"
  in
  [
    Application (Application dpair a') (Binder Lambda (NamedBinder id) Wildcard b') |
    a' <- desugarWithContext xs a,
    b' <- desugarWithContext (NamedBinder id :: xs) b
  ]
desugarWithContext xs (DependentPairConstructorSugar e1 e2) = 
  let
    dpairconstructor : Desugared NoHole
    dpairconstructor = Constant $ MkIdentifier NameId "MkDPair"
  in
  [
    Application (Application dpairconstructor e1') e2' |
    e1' <- desugarWithContext xs e1,
    e2' <- desugarWithContext xs e2
  ]  
desugarWithContext xs (EqualSugar e1 e2) = 
  let
    equal : Desugared NoHole
    equal = Constant $ MkIdentifier OperatorId "==="
  in
  [
    Application (Application equal e1') e2' |
    e1' <- desugarWithContext xs e1,
    e2' <- desugarWithContext xs e2
  ]     
desugarWithContext xs (MemberSugar x f) = 
  [
    Application (Constant $ MemberToIdentifier f) x' |
    x' <- desugarWithContext xs x
  ]

-- === the main function ===
export
desugar : Sugared Expr -> List1 (Desugared NoHole)
desugar x = desugarWithContext [] x


getImplicitList : List Identifier -> Sugared Expr -> List Identifier
getImplicitList ids (IdentifierTerm x@(MkIdentifier NameId str)) =
  let
    isHeadLower : String -> Bool
    isHeadLower str =
      case unpack str of
        [] => False
        (x :: xs) => isLower x
  in
  if isHeadLower str && not (x `elem` ids)
    then [x]
    else []
getImplicitList _ (IdentifierTerm (MkIdentifier _ _)) = []
getImplicitList ids (Application (IdentifierTerm _) e) = getImplicitList ids e
getImplicitList ids (Application e1 e2) = nub $ getImplicitList ids e1 ++ getImplicitList ids e2
getImplicitList ids (Arrow _ ty e) = nub $ getImplicitList ids ty ++ getImplicitList ids e
getImplicitList ids (SignatureArrow _ (Signature id ty) e) = 
    nub $ getImplicitList ids ty ++ getImplicitList (id :: ids) e
getImplicitList ids (BracketArrow (Signature id ty) e) =
    nub $ getImplicitList ids ty ++ getImplicitList (id :: ids) e
getImplicitList ids (AnonymousFunction _ e) = getImplicitList ids e
getImplicitList _ (Literal _ _) = []
getImplicitList _ Wildcard = []
getImplicitList _ UnitSugar = []
getImplicitList ids (PairSugar e1 e2) = nub $ getImplicitList ids e1 ++ getImplicitList ids e2
getImplicitList ids (OpInfixSugar e1 _ e2) = nub $ getImplicitList ids e1 ++ getImplicitList ids e2
getImplicitList ids (InfixSugar e1 _ e2) = nub $ getImplicitList ids e1 ++ getImplicitList ids e2
getImplicitList ids (DependentPairSugar id ty e) =
    nub $ getImplicitList ids ty ++ getImplicitList (id :: ids) e
getImplicitList ids (DependentPairConstructorSugar e1 e2) =
    nub $ getImplicitList ids e1 ++ getImplicitList ids e2
getImplicitList ids (EqualSugar e1 e2) = nub $ getImplicitList ids e1 ++ getImplicitList ids e2
getImplicitList ids (MemberSugar e _) = nub $ getImplicitList ids e

addImplictsFromList : List Identifier -> Sugared Expr -> Sugared Expr
addImplictsFromList [] e = e
addImplictsFromList (id :: ids) e = addImplictsFromList ids $ BracketArrow (Signature id Wildcard) e

addImplicits : Sugared Expr -> Sugared Expr
addImplicits e = addImplictsFromList (getImplicitList [] e) e

export
desugarType : Sugared Expr -> List1 (Desugared NoHole)
desugarType e = desugar $ addImplicits e

export
desugarSig : Sugared Sig -> List1 (DesugaredSignature NoHole)
desugarSig (Signature x y) =
  [
    MkDSig x y' |
    y' <- desugarType y
  ]
