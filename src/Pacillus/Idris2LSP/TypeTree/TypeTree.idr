module Pacillus.Idris2LSP.TypeTree.TypeTree

import Data.Vect

import Pacillus.Idris2LSP.Parser.Basic
import Pacillus.Idris2LSP.TypeTree.Output

-- replacing the index which were bound implicitly to holes
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
replaceIndex index holes_count e@(Assumption _ _) = e

-- converting Desugared into WithHole by converting all WildCard to holes
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

-- convert implicit binders to holes
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
openImplicitHoles holes_count e@(Assumption _ _) = (holes_count, e)

Constraints : Type
Constraints = List (Desugared WithHole, Desugared WithHole)

unifyError : Desugared WithHole -> Desugared WithHole -> Either String Constraints
unifyError x y =  Left "unification failed between \{show x} and \{show y}"

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
substituteToImplicit k sub_with e@(Assumption _ _) = e

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
    ty' = substituteToIndex k sub_with ty
    e' = substituteToIndex (S k) sub_with e
  in
    Binder t id ty' e'
substituteToIndex k sub_with e@(Literal _ _) = e
substituteToIndex k sub_with e@(ImplicitHole _ _) = e
substituteToIndex k sub_with e@(Assumption _ _) = e

substituteToConstraints : Nat -> Desugared WithHole -> Constraints -> Constraints
substituteToConstraints k sub_with xs = map (\constraint => (substituteToImplicit k sub_with (fst constraint), substituteToImplicit k sub_with $ snd constraint)) xs

binderToAssumptionMain : Nat -> Desugared WithHole -> Desugared WithHole
binderToAssumptionMain _ e@(Constant _) = e
binderToAssumptionMain j e@(Index id k) with (j == k)
  binderToAssumptionMain j e@(Index id k) | False = e
  binderToAssumptionMain j e@(Index id k) | True = Assumption id 0
binderToAssumptionMain k (Application f x) = Application (binderToAssumptionMain k f) (binderToAssumptionMain k x)
binderToAssumptionMain k (Binder bty id ty e) = Binder bty id (binderToAssumptionMain k ty) (binderToAssumptionMain (S k) e)
binderToAssumptionMain _ e@(Literal _ _) = e
binderToAssumptionMain _ e@(ImplicitHole _ _) = e
binderToAssumptionMain _ (Assumption id k) = Assumption id (S k)

binderToAssumption : Desugared WithHole -> Desugared WithHole
binderToAssumption x = binderToAssumptionMain 0 x

assumptionToBinderMain : Nat -> Desugared WithHole -> Desugared WithHole
assumptionToBinderMain _ e@(Constant _) = e
assumptionToBinderMain _ e@(Index _ _) = e
assumptionToBinderMain k (Application x y) = Application (assumptionToBinderMain k x) (assumptionToBinderMain k y)
assumptionToBinderMain k (Binder bty id ty e) = Binder bty id (assumptionToBinderMain k ty) (assumptionToBinderMain (S k) e)
assumptionToBinderMain k e@(Literal t x) = e
assumptionToBinderMain k e@(ImplicitHole _ _) = e
assumptionToBinderMain k (Assumption id 0) = Index id k
assumptionToBinderMain j (Assumption id (S k)) = Assumption id k

assumptionToBinder : Desugared WithHole -> Desugared WithHole
assumptionToBinder x = assumptionToBinderMain 0 x

unify : Constraints -> Either String Constraints
unify [] = Right []
unify ((e1@(Constant x), e2@(Constant y)) :: xs) with (x == y)
  unify ((e1@(Constant _), e2@(Constant _)) :: _) | False = unifyError e1 e2
  unify ((e1@(Constant _), e2@(Constant _)) :: xs) | True = unify xs
unify ((e1@(Index _ j), e2@(Index _ k)) :: xs) with (j == k)
  unify ((e1@(Index _ j), e2@(Index _ k)) :: xs) | False = unifyError e1 e2
  unify ((e1@(Index _ j), e2@(Index _ k)) :: xs) | True = unify xs
unify ((Application f x, Application g y) :: xs) = unify $ (f, g) :: (x, y) :: xs
unify ((el@(Binder t1 _ ty1 e1), er@(Binder t2 _ ty2 e2)) :: xs) with (t1 == t2)
  unify ((el@(Binder _ _ _ _), er@(Binder _ _ _ _)) :: xs) | False = unifyError el er
  unify ((el@(Binder _ _ ty1 e1), er@(Binder _ _ ty2 e2)) :: xs) | True = 
    unify $ (ty1, ty2) :: (e1, e2) :: xs
unify ((e1@(Literal IntegerL x), e2@(Literal IntegerL y)) :: xs) with (x == y)
  unify ((e1@(Literal IntegerL x), e2@(Literal IntegerL y)) :: xs) | False = unifyError e1 e2
  unify ((e1@(Literal IntegerL x), e2@(Literal IntegerL y)) :: xs) | True = unify xs
unify ((e1@(Literal DoubleL x), e2@(Literal DoubleL y)) :: xs) with (x == y)
  unify ((e1@(Literal DoubleL x), e2@(Literal DoubleL y)) :: xs) | False = unifyError e1 e2
  unify ((e1@(Literal DoubleL x), e2@(Literal DoubleL y)) :: xs) | True = unify xs
unify ((e1@(Literal CharL x), e2@(Literal CharL y)) :: xs) with (x == y)
  unify ((e1@(Literal CharL x), e2@(Literal CharL y)) :: xs) | False = unifyError e1 e2
  unify ((e1@(Literal CharL x), e2@(Literal CharL y)) :: xs) | True = unify xs
unify ((e1@(Literal StringL x), e2@(Literal StringL y)) :: xs) with (x == y)
  unify ((e1@(Literal StringL x), e2@(Literal StringL y)) :: xs) | False = unifyError e1 e2
  unify ((e1@(Literal StringL x), e2@(Literal StringL y)) :: xs) | True = unify xs
unify ((l@(ImplicitHole _ k), r) :: xs) =
  let
    substitutedToImplicit = substituteToConstraints k r xs -- TODO Maybe 
  in
    unify substitutedToImplicit >>= (\xs => Right $ (l, r) :: xs)
unify ((l, r@(ImplicitHole _ k)) :: xs) = 
  let
    substitutedToImplicit = substituteToConstraints k l xs
  in
    unify substitutedToImplicit >>= (\xs => Right $ (l, r) :: xs)
unify ((e1@(Assumption _ j), e2@(Assumption _ k)) :: xs) with (j == k)
  unify ((e1@(Assumption _ j), e2@(Assumption _ k)) :: xs) | False = unifyError e1 e2
  unify ((e1@(Assumption _ j), e2@(Assumption _ k)) :: xs) | True = unify xs
unify ((e1, e2) :: xs) = unifyError e1 e2

simpleEval : Desugared WithHole -> Desugared WithHole
simpleEval e@(Constant x) = e
simpleEval e@(Index x k) = e
simpleEval (Application (Binder Lambda _ _ applying) applied) = substituteToIndex 0 applied applying
simpleEval (Application f x) = Application (simpleEval f) (simpleEval x)
simpleEval (Binder bty id ty e) = Binder bty id (simpleEval ty) (simpleEval e)
simpleEval e@(Literal t x) = e
simpleEval e@(ImplicitHole x k) = e
simpleEval e@(Assumption x k) = e

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
    constraints <- unify [(argty, ty2)]
    retty'' <- applyConstraints constraints retty'
    Right $ MkExprSignature name (simpleEval retty'')
getAppliedType (MkExprSignature _ _) (MkExprSignature _ _) = Left "Non appliable form found in application"

skipAuto : TypeTree -> TypeTree
skipAuto x with (getSubgoal x)
  skipAuto x | (MkExprSignature id (Binder Auto _ _ e)) = Subgoal [x] (MkExprSignature id e)
  skipAuto x | (MkExprSignature _ _) = x

typeId : ExprSignature
typeId = MkExprSignature (Constant $ MkIdentifier NameId "Type") $ Constant $ MkIdentifier NameId "Type"

record GetPartialTypeEnv where
  constructor MkGetPartialTypeEnv
  assumptionTypes : List (Desugared WithHole)
  signatures : List (DesugaredSignature WithHole)
  holesCount : Nat

typeOfConstant : Identifier -> List (DesugaredSignature WithHole) -> Either String (Desugared WithHole)
typeOfConstant x [] = Left $ "could not find the type of identifier " ++ show x
typeOfConstant x (MkDSig y ty :: sigs) = 
  if x == y
    then Right ty
    else typeOfConstant x sigs

getPartialTypeMain : GetPartialTypeEnv -> Desugared WithHole -> Either String (GetPartialTypeEnv, TypeTree)
getPartialTypeMain env e@(Constant x) =
  do
    ty <- typeOfConstant x env.signatures
    Right $ (env, Start $ MkExprSignature e ty)
getPartialTypeMain env e@(Index _ k) with (getAt k env.assumptionTypes)
  getPartialTypeMain env e@(Index _ k) | Nothing = Left "corrputedly bound variable found"
  getPartialTypeMain env e@(Index _ k) | (Just x) = Right $ (env, Start $ MkExprSignature e x)
getPartialTypeMain env e@(Application f x) =
  do  
    (env', f') <- getPartialTypeMain env f
    (env'', x') <- getPartialTypeMain env' x
    -----
    f'' <- Right $ skipAuto f'
    x'' <- Right $ skipAuto x'
    ----- ==> let f'' = skipAuto f'; x'' = skipAuto x' in
    appty <- getAppliedType (getSubgoal f'') (getSubgoal x'')
    Right $ (env'', Subgoal [f'', x''] appty)
getPartialTypeMain env e@(Binder Pi y z w) = Right $ (env, Start typeId)
getPartialTypeMain env e@(Binder Lambda id ty e2) =
  do
    (env', typetree) <- getPartialTypeMain ({assumptionTypes $= (ty ::)} env) (binderToAssumption e2)
    retty <- Right $ assumptionToBinder $ (getSubgoal typetree).type
    Right $ (env', Subgoal [typetree] $ (MkExprSignature e (Binder Pi id ty retty)))
getPartialTypeMain env e@(Binder Auto y z w) = Right $ (env, Start typeId)
getPartialTypeMain env e@(Binder Implicit y z w) = Right $ (env, Start typeId)
getPartialTypeMain env e@(Literal IntegerL x) = Right $ (env, Start $ MkExprSignature e $ Constant $ MkIdentifier NameId "Integer")
getPartialTypeMain env e@(Literal DoubleL x) = Right $ (env, Start $ MkExprSignature e $ Constant $ MkIdentifier NameId "Double")
getPartialTypeMain env e@(Literal CharL x) = Right $ (env, Start $ MkExprSignature e $ Constant $ MkIdentifier NameId "Char")
getPartialTypeMain env e@(Literal StringL x) = Right $ (env, Start $ MkExprSignature e $ Constant $ MkIdentifier NameId "String")
getPartialTypeMain env e@(ImplicitHole _ k) = Right $ ({holesCount $= S} env, Start $ MkExprSignature e $ ImplicitHole (MkIdentifier NameId "_") env.holesCount)
getPartialTypeMain env e@(Assumption _ k) with (getAt k env.assumptionTypes)
  getPartialTypeMain env e@(Assumption _ k) | Nothing = Left "corrputed assumption"
  getPartialTypeMain env e@(Assumption _ k) | (Just ty) = Right $ (env, Start (MkExprSignature e ty))

convertSigs : Nat -> List (DesugaredSignature WithHole) -> List (DesugaredSignature NoHole) -> (Nat, List (DesugaredSignature WithHole))
convertSigs n acc [] = (n, acc)
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
    (n'', sigs') = convertSigs n' [] sigs
  in
  do
    map snd $ getPartialTypeMain (MkGetPartialTypeEnv [] sigs' n'') x''