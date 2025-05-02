module Pacillus.Idris2LSP.TypeTree.TypeTree

import Data.Vect

import Pacillus.Idris2LSP.Parser.Basic

public export
data ExprSignature : Type where
    MkExprSignature : Desugared WithHole -> Desugared WithHole -> ExprSignature

(.type) : ExprSignature -> Desugared WithHole
(MkExprSignature e ty).type = ty

public export
data ReconsTree : Type where
    Start : ExprSignature -> ReconsTree
    Subgoal : List ReconsTree -> ExprSignature -> ReconsTree

getSubgoal : ReconsTree -> ExprSignature
getSubgoal (Start x) = x
getSubgoal (Subgoal xs x) = x


replaceIndex : Nat -> Nat -> Desugared WithHole -> Desugared WithHole
replaceIndex index holes_count (Constant x y) = Constant x y
replaceIndex index holes_count (Index k) with (index == k)
  replaceIndex index holes_count (Index k) | False = Index k
  replaceIndex index holes_count (Index k) | True = ImplicitHole holes_count
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
replaceIndex index holes_count (ImplicitHole k) = ImplicitHole k

toWithHole : Nat -> Desugared NoHole -> (Nat, Desugared WithHole)
toWithHole holes_count (Constant x y) = (holes_count, Constant x y)
toWithHole holes_count (Index k) = (holes_count, Index k)
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
toWithHole holes_count (Binder Auto id ty e) = toWithHole holes_count e
toWithHole holes_count_a (Binder Implicit id ty e) = 
  let
    (holes_count_b, ty') = toWithHole holes_count_a ty
    (holes_count_c, e') = toWithHole holes_count_b e
  in
    (holes_count_c, (Binder Implicit id ty' e'))
toWithHole holes_count (Literal t x) = (holes_count, Literal t x)
toWithHole holes_count WildCard = (S holes_count, ImplicitHole holes_count)

-- toWithHole' : Desugared NoHole -> (Nat, Desugared WithHole)
-- toWithHole' = toWithHole 0

openImplicitHoles : Nat -> Desugared WithHole -> (Nat, Desugared WithHole)
openImplicitHoles holes_count e@(Constant _ _) = (holes_count, e)
openImplicitHoles holes_count e@(Index _) = (holes_count, e)
openImplicitHoles holes_count e@(Application _ _) = (holes_count, e)
openImplicitHoles holes_count e@(Binder Pi _ _ _) = (holes_count, e)
openImplicitHoles holes_count e@(Binder Lambda _ _ _) = (holes_count, e)
openImplicitHoles holes_count (Binder Auto _ _ e) = openImplicitHoles holes_count e
openImplicitHoles holes_count (Binder Implicit _ _ e) = 
    openImplicitHoles (S holes_count) $ replaceIndex 0 holes_count e
openImplicitHoles holes_count e@(Literal _ _) = (holes_count, e)
openImplicitHoles holes_count e@(ImplicitHole _) = (holes_count, e)

Constraints : Type
Constraints = List (Desugared WithHole, Desugared WithHole)

unifyError : Either String Constraints
unifyError =  Left "unexpected error something went wrong in unification"

substitute : Nat -> Desugared WithHole -> Desugared WithHole -> Desugared WithHole
substitute k sub_with e@(Constant x y) = e
substitute k sub_with e@(Index j) = e
substitute k sub_with (Application f x) =
  let
    f' = substitute k sub_with f
    x' = substitute k sub_with x
  in
    Application f' x'
substitute k sub_with (Binder t id ty e) = 
  let
    ty' = substitute k sub_with ty
    e' = substitute k sub_with e
  in
    Binder t id ty' e'
substitute k sub_with (Literal t x) = Literal t x
substitute k sub_with e@(ImplicitHole j) with (k == j)
  substitute k sub_with e@(ImplicitHole j) | False = e
  substitute k sub_with e@(ImplicitHole j) | True = sub_with

substituteToConstraints : Nat -> Desugared WithHole -> Constraints -> Constraints
substituteToConstraints k sub_with xs = map (\constraint => (substitute k sub_with (fst constraint), substitute k sub_with $ snd constraint)) xs
-- substituteToConstraints R k sub_with xs = map (\constraint => (fst constraint, substitute k sub_with (snd constraint))) xs

unify : Constraints -> Either String Constraints
unify [] = Right []
unify ((Constant t1 x, Constant t2 y) :: xs) with (sameIdGroup t1 t2 && x == y)
  unify ((Constant _ _, Constant _ _) :: _) | False = unifyError
  unify ((Constant _ _, Constant _ _) :: xs) | True = unify xs
unify ((Index j, Index k) :: xs) with (j == k)
  unify ((Index j, Index k) :: xs) | False = unifyError
  unify ((Index j, Index k) :: xs) | True = unify xs
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
unify ((ImplicitHole j, ImplicitHole k) :: xs) = unify xs
unify ((l@(ImplicitHole k), r) :: xs) =
  let
    substituted = substituteToConstraints k r xs -- TODO Maybe 
  in
    unify substituted >>= (\xs => Right $ (l, r) :: xs)
unify ((l, r@(ImplicitHole k)) :: xs) = 
  let
    substituted = substituteToConstraints k l xs
  in
    unify substituted >>= (\xs => Right $ (l, r) :: xs)
unify ((_, _) :: xs) = unifyError

applyConstraints : Constraints -> Desugared WithHole -> Either String (Desugared WithHole)
applyConstraints [] e = Right e
applyConstraints ((ImplicitHole k, x) :: xs) e = 
  let
    xs' = substituteToConstraints k x xs
    e' = substitute k e
  in
   applyConstraints xs' e
applyConstraints ((x, ImplicitHole k) :: xs) e = 
  let
    xs' = substituteToConstraints k x xs
    e' = substitute k e
  in
    applyConstraints xs' e
applyConstraints ((_, _) :: xs) e = Left "Unexpected constraints found after unifciation"

getAppliedType : ExprSignature -> ExprSignature -> Either String ExprSignature
getAppliedType (MkExprSignature f (Binder Pi _ argty retty)) (MkExprSignature x ty2) = 
  let
    retty' = substitute 0 x retty
    name = Application f x
  in
  do 
    constraints <-  unify [(argty, ty2)]
    retty'' <- applyConstraints constraints retty'
    Right $ MkExprSignature name retty''
getAppliedType (MkExprSignature _ _) (MkExprSignature _ _) = Left "Non appliable form found in application"

typeId : ExprSignature
typeId = MkExprSignature (Constant NameId $ MkIdentifier "Type") $ Constant NameId $ MkIdentifier "Type"

getPartialTypeMain : List (Desugared WithHole) -> List (DesugaredSignature WithHole) -> Desugared WithHole -> Either String ReconsTree
getPartialTypeMain _ [] (Constant x y) = Left $ "could not find the type of identifier " ++ show y
getPartialTypeMain binder_types (MkDSig t1 x ty :: xs) e@(Constant t2 y) = 
  if sameIdGroup t1 t2 && x == y
    then Right $ Start $ MkExprSignature e ty
    else getPartialTypeMain binder_types xs e
getPartialTypeMain binder_types _ e@(Index k) with (getAt k binder_types)
  getPartialTypeMain binder_types _ e@(Index k) | Nothing = Left "corrputedly bound variable found"
  getPartialTypeMain binder_types _ e@(Index k) | (Just x) = Right $ Start $ MkExprSignature e x -- Eq Int => \y : b => 1
getPartialTypeMain binder_types sigs e@(Application f x) = 
  do
    f' <- getPartialTypeMain binder_types sigs f
    x' <- getPartialTypeMain binder_types sigs x
    appty <- getAppliedType (getSubgoal f') (getSubgoal x')
    Right $ Subgoal [f', x'] appty
getPartialTypeMain binder_types _ e@(Binder Pi y z w) = Right $ Start typeId
getPartialTypeMain binder_types sigs e@(Binder Lambda id ty e2) = 
  do
    e2' <- getPartialTypeMain (ty :: binder_types) sigs e2
    Right $ Subgoal [e2'] $ (MkExprSignature e (Binder Pi id ty $ (getSubgoal e2').type))
getPartialTypeMain binder_types _ e@(Binder Auto y z w) = Right $ Start typeId
getPartialTypeMain binder_types _ e@(Binder Implicit y z w) = Right $ Start typeId
getPartialTypeMain _ _ e@(Literal IntegerL x) = Right $ Start $ MkExprSignature e $ Constant NameId $ MkIdentifier "Integer"
getPartialTypeMain _ _ e@(Literal DoubleL x) = Right $ Start $ MkExprSignature e $ Constant NameId $ MkIdentifier "Double"
getPartialTypeMain _ _ e@(Literal CharL x) = Right $ Start $ MkExprSignature e $ Constant NameId $ MkIdentifier "Char"
getPartialTypeMain _ _ e@(Literal StringL x) = Right $ Start $ MkExprSignature e $ Constant NameId $ MkIdentifier "String"
getPartialTypeMain _ _ e@(ImplicitHole k) = ?getPartialTypeMain_rhs_5

convertSigs : Nat -> List (DesugaredSignature WithHole) -> List (DesugaredSignature NoHole) -> List (DesugaredSignature WithHole)
convertSigs n acc [] = acc
convertSigs n acc (MkDSig t id x :: xs) =
  let
    (n', x') = toWithHole n x
    (n'', x'') = openImplicitHoles n' x'
  in
    convertSigs n'' (MkDSig t id x'' :: acc) xs

export
getPartialType : List (DesugaredSignature NoHole) -> Desugared NoHole -> Either String ReconsTree
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