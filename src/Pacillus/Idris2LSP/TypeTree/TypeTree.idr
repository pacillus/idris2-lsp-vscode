module Pacillus.Idris2LSP.TypeTree.TypeTree

import Data.Vect

import Pacillus.Idris2LSP.Parser.Basic

public export
data ExprSignature : Type where
    MkExprSignature : Desugared WithHole -> Desugared WithHole -> ExprSignature

(.type) : ExprSignature -> Desugared WithHole
(MkExprSignature e ty).type = ty

public export
data TypeTree : Type where
    Start : ExprSignature -> TypeTree
    Subgoal : List TypeTree -> ExprSignature -> TypeTree

getSubgoal : TypeTree -> ExprSignature
getSubgoal (Start x) = x
getSubgoal (Subgoal xs x) = x


replaceIndex : Nat -> Nat -> Desugared WithHole -> Desugared WithHole
replaceIndex index holes_count (Constant x) = Constant x
replaceIndex index holes_count e@(Index name k) with (index == k)
  replaceIndex index holes_count e@(Index _ _) | False = e
  replaceIndex index holes_count e@(Index name k) | True = ImplicitHole name holes_count
replaceIndex index holes_count (Application f x) = 
  let
    f' = replaceIndex index holes_count f
    x' = replaceIndex index holes_count x
  in
    Application f' x'
replaceIndex index holes_count (Binder t id ty e) = 
  let
    ty' = replaceIndex index holes_count ty
    e' = replaceIndex (S index) holes_count e
  in
    Binder t id ty' e'
replaceIndex index holes_count (Literal x y) = Literal x y
replaceIndex index holes_count e@(ImplicitHole _ _) = e

toWithHole : Nat -> Desugared NoHole -> (Nat, Desugared WithHole)
toWithHole holes_count (Constant x) = (holes_count, Constant x)
toWithHole holes_count (Index name k) = (holes_count, Index name k)
toWithHole holes_count_a (Application f x) = 
  let
    (holes_count_b, f') = toWithHole holes_count_a f
    (holes_count_c, x') = toWithHole holes_count_b x
  in
    (holes_count_c, Application f' x')
toWithHole holes_count_a (Binder Pi id ty e) = 
  let
    (holes_count_b, ty') = toWithHole holes_count_a ty
    (holes_count_c, e') = toWithHole holes_count_b e
  in
    (holes_count_c, Binder Pi id ty' e')
toWithHole holes_count_a (Binder Lambda id ty e) =
  let
    (holes_count_b, ty') = toWithHole holes_count_a ty
    (holes_count_c, e') = toWithHole holes_count_b e
  in
    (holes_count_c, Binder Lambda id ty' e')
toWithHole holes_count_a (Binder Auto id ty e) = 
  let
    (holes_count_b, ty') = toWithHole holes_count_a ty
    (holes_count_c, e') = toWithHole holes_count_b e
  in
    (holes_count_c, (Binder Auto id ty' e'))
toWithHole holes_count_a (Binder Implicit id ty e) = 
  let
    (holes_count_b, ty') = toWithHole holes_count_a ty
    (holes_count_c, e') = toWithHole holes_count_b e
  in
    (holes_count_c, (Binder Implicit id ty' e'))
toWithHole holes_count (Literal t x) = (holes_count, Literal t x)
toWithHole holes_count Wildcard = (S holes_count, ImplicitHole (MkIdentifier NameId "_") holes_count)

-- toWithHole' : Desugared NoHole -> (Nat, Desugared WithHole)
-- toWithHole' = toWithHole 0

openImplicitHoles : Nat -> Desugared WithHole -> (Nat, Desugared WithHole)
openImplicitHoles holes_count e@(Constant _) = (holes_count, e)
openImplicitHoles holes_count e@(Index _ _) = (holes_count, e)
openImplicitHoles holes_count e@(Application _ _) = (holes_count, e)
openImplicitHoles holes_count (Binder Pi id ty e) =
  let
    (holes_count', e') = openImplicitHoles holes_count e
  in
    (holes_count', Binder Pi id ty e')
openImplicitHoles holes_count e@(Binder Lambda _ _ _) = (holes_count, e)
openImplicitHoles holes_count (Binder Auto id ty e) = 
  let
    (holes_count', e') = openImplicitHoles holes_count e
  in
    (holes_count', Binder Auto id ty e')
openImplicitHoles holes_count (Binder Implicit _ _ e) = 
    openImplicitHoles (S holes_count) $ replaceIndex 0 holes_count e
openImplicitHoles holes_count e@(Literal _ _) = (holes_count, e)
openImplicitHoles holes_count e@(ImplicitHole _ _) = (holes_count, e)

Constraints : Type
Constraints = List (Desugared WithHole, Desugared WithHole)

unifyError : Either String Constraints
unifyError =  Left "unexpected error something went wrong in unification"

substituteToImplicit : Nat -> Desugared WithHole -> Desugared WithHole -> Desugared WithHole
substituteToImplicit k sub_with e@(Constant _) = e
substituteToImplicit k sub_with e@(Index _ _) = e
substituteToImplicit k sub_with (Application f x) =
  let
    f' = substituteToImplicit k sub_with f
    x' = substituteToImplicit k sub_with x
  in
    Application f' x'
substituteToImplicit k sub_with (Binder t id ty e) = 
  let
    ty' = substituteToImplicit k sub_with ty
    e' = substituteToImplicit k sub_with e
  in
    Binder t id ty' e'
substituteToImplicit k sub_with (Literal t x) = Literal t x
substituteToImplicit k sub_with e@(ImplicitHole _ j) with (k == j)
  substituteToImplicit k sub_with e@(ImplicitHole _ j) | False = e
  substituteToImplicit k sub_with e@(ImplicitHole _ j) | True = sub_with

substituteToIndex : Nat -> Desugared WithHole -> Desugared WithHole -> Desugared WithHole
substituteToIndex k sub_with e@(Constant _) = e
substituteToIndex k sub_with e@(Index _ j) with (k == j)
  substituteToIndex k sub_with e@(Index _ j) | False = e
  substituteToIndex k sub_with e@(Index _ j) | True = sub_with
substituteToIndex k sub_with (Application f x) =
  let
    f' = substituteToIndex k sub_with f
    x' = substituteToIndex k sub_with x
  in
    Application f' x'
substituteToIndex k sub_with (Binder t id ty e) = 
  let
    ty' = substituteToIndex (S k) sub_with ty
    e' = substituteToIndex (S k) sub_with e
  in
    Binder t id ty' e'
substituteToIndex k sub_with e@(Literal _ _) = e
substituteToIndex k sub_with e@(ImplicitHole _ _) = e

substituteToConstraints : Nat -> Desugared WithHole -> Constraints -> Constraints
substituteToConstraints k sub_with xs = map (\constraint => (substituteToImplicit k sub_with (fst constraint), substituteToImplicit k sub_with $ snd constraint)) xs
-- substituteToConstraints R k sub_with xs = map (\constraint => (fst constraint, substituteToImplicit k sub_with (snd constraint))) xs

unify : Constraints -> Either String Constraints
unify [] = Right []
unify ((Constant x, Constant y) :: xs) with (x == y)
  unify ((Constant _, Constant _) :: _) | False = unifyError
  unify ((Constant _, Constant _) :: xs) | True = unify xs
unify ((Index _ j, Index _ k) :: xs) with (j == k)
  unify ((Index _ j, Index _ k) :: xs) | False = unifyError
  unify ((Index _ j, Index _ k) :: xs) | True = unify xs
unify ((Application f x, Application g y) :: xs) = unify $ (f, g) :: (x, y) :: xs
unify ((Binder t1 _ ty1 e1, Binder t2 _ ty2 e2) :: xs) with (t1 == t2)
  unify ((Binder _ _ _ _, Binder _ _ _ _) :: xs) | False = unifyError
  unify ((Binder _ _ ty1 e1, Binder _ _ ty2 e2) :: xs) | True = 
    unify $ (ty1, ty2) :: (e1, e2) :: xs
unify ((Literal IntegerL x, Literal IntegerL y) :: xs) with (x == y)
  unify ((Literal IntegerL x, Literal IntegerL y) :: xs) | False = unifyError
  unify ((Literal IntegerL x, Literal IntegerL y) :: xs) | True = unify xs
unify ((Literal DoubleL x, Literal DoubleL y) :: xs) with (x == y)
  unify ((Literal DoubleL x, Literal DoubleL y) :: xs) | False = unifyError
  unify ((Literal DoubleL x, Literal DoubleL y) :: xs) | True = unify xs
unify ((Literal CharL x, Literal CharL y) :: xs) with (x == y)
  unify ((Literal CharL x, Literal CharL y) :: xs) | False = unifyError
  unify ((Literal CharL x, Literal CharL y) :: xs) | True = unify xs
unify ((Literal StringL x, Literal StringL y) :: xs) with (x == y)
  unify ((Literal StringL x, Literal StringL y) :: xs) | False = unifyError
  unify ((Literal StringL x, Literal StringL y) :: xs) | True = unify xs
-- unify ((l@(ImplicitHole _ _), r@(ImplicitHole _ _)) :: xs) = (::) unify xs
unify ((l@(ImplicitHole _ k), r) :: xs) =
  let
    substituteToImplicitd = substituteToConstraints k r xs -- TODO Maybe 
  in
    unify substituteToImplicitd >>= (\xs => Right $ (l, r) :: xs)
unify ((l, r@(ImplicitHole _ k)) :: xs) = 
  let
    substituteToImplicitd = substituteToConstraints k l xs
  in
    unify substituteToImplicitd >>= (\xs => Right $ (l, r) :: xs)
unify ((_, _) :: xs) = Left "test" --unifyError

applyConstraints : Constraints -> Desugared WithHole -> Either String (Desugared WithHole)
applyConstraints [] e = Right e
applyConstraints ((ImplicitHole _ k, x) :: xs) e = 
  let
    xs' = substituteToConstraints k x xs
    e' = substituteToImplicit k x e
  in
   applyConstraints xs' e'
applyConstraints ((x, ImplicitHole _ k) :: xs) e = 
  let
    xs' = substituteToConstraints k x xs
    e' = substituteToImplicit k x e
  in
    applyConstraints xs' e'
applyConstraints ((_, _) :: xs) e = Left "Unexpected constraints found after unifciation"

getAppliedType : ExprSignature -> ExprSignature -> Either String ExprSignature
getAppliedType (MkExprSignature f (Binder Pi _ argty retty)) (MkExprSignature x ty2) = 
  let
    retty' = substituteToIndex 0 x retty
    name = Application f x
  in
  do 
    constraints <-  unify [(argty, ty2)]
    retty'' <- applyConstraints constraints retty'
    Right $ MkExprSignature name retty''
getAppliedType (MkExprSignature _ _) (MkExprSignature _ _) = Left "Non appliable form found in application"

skipAuto : TypeTree -> TypeTree
skipAuto x with (getSubgoal x)
  skipAuto x | (MkExprSignature id (Binder Auto _ _ e)) = Subgoal [x] (MkExprSignature id e)
  skipAuto x | (MkExprSignature _ _) = x

typeId : ExprSignature
typeId = MkExprSignature (Constant $ MkIdentifier NameId "Type") $ Constant $ MkIdentifier NameId "Type"

getPartialTypeMain : List (Desugared WithHole) -> List (DesugaredSignature WithHole) -> Desugared WithHole -> Either String TypeTree
getPartialTypeMain _ [] (Constant y) = Left $ "could not find the type of identifier " ++ show y
getPartialTypeMain binder_types (MkDSig x ty :: xs) e@(Constant y) = 
  if x == y
    then Right $ Start $ MkExprSignature e ty
    else getPartialTypeMain binder_types xs e
getPartialTypeMain binder_types _ e@(Index _ k) with (getAt k binder_types)
  getPartialTypeMain binder_types _ e@(Index _ k) | Nothing = Left "corrputedly bound variable found"
  getPartialTypeMain binder_types _ e@(Index _ k) | (Just x) = Right $ Start $ MkExprSignature e x -- Eq Int => \y : b => 1
getPartialTypeMain binder_types sigs e@(Application f x) = 
  do  
    f' <- getPartialTypeMain binder_types sigs f
    x' <- getPartialTypeMain binder_types sigs x
    -----
    f'' <- Right $ skipAuto f'
    x'' <- Right $ skipAuto x'
    -- ==> let f'' = skipAuto f'; x'' = skipAuto x' in
    appty <- getAppliedType (getSubgoal f'') (getSubgoal x'')
    Right $ Subgoal [f'', x''] appty

getPartialTypeMain binder_types _ e@(Binder Pi y z w) = Right $ Start typeId
getPartialTypeMain binder_types sigs e@(Binder Lambda id ty e2) = 
  do
    e2' <- getPartialTypeMain (ty :: binder_types) sigs e2
    Right $ Subgoal [e2'] $ (MkExprSignature e (Binder Pi id ty $ (getSubgoal e2').type))
getPartialTypeMain binder_types _ e@(Binder Auto y z w) = Right $ Start typeId
getPartialTypeMain binder_types _ e@(Binder Implicit y z w) = Right $ Start typeId
getPartialTypeMain _ _ e@(Literal IntegerL x) = Right $ Start $ MkExprSignature e $ Constant $ MkIdentifier NameId "Integer"
getPartialTypeMain _ _ e@(Literal DoubleL x) = Right $ Start $ MkExprSignature e $ Constant $ MkIdentifier NameId "Double"
getPartialTypeMain _ _ e@(Literal CharL x) = Right $ Start $ MkExprSignature e $ Constant $ MkIdentifier NameId "Char"
getPartialTypeMain _ _ e@(Literal StringL x) = Right $ Start $ MkExprSignature e $ Constant $ MkIdentifier NameId "String"
getPartialTypeMain _ _ e@(ImplicitHole _ k) = ?getPartialTypeMain_rhs_5

convertSigs : Nat -> List (DesugaredSignature WithHole) -> List (DesugaredSignature NoHole) -> List (DesugaredSignature WithHole)
convertSigs n acc [] = acc
convertSigs n acc (MkDSig id x :: xs) =
  let
    (n', x') = toWithHole n x
    (n'', x'') = openImplicitHoles n' x'
  in
    convertSigs n'' (MkDSig id x'' :: acc) xs

export
getPartialType : List (DesugaredSignature NoHole) -> Desugared NoHole -> Either String TypeTree
getPartialType sigs x = 
  let
    (n, x') = toWithHole 0 x
    (n', x'') = openImplicitHoles n x'
    -- listWithHole =
    sigs' = convertSigs n' [] sigs
  in
    getPartialTypeMain [] sigs' x''





-- f : a -> Type
--  : (\x =>  f x) x_a ==> f x_a
--  : {0 g : Type} -> (\y => g) x_a ==> {0 g : Type} -> g

--