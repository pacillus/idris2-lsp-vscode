import Data.Vect

vec1 : Vect 1 Nat
vec1 = [1]

vec2 : Vect 2 Nat
vec2 = [2, 3]

vec3 : Vect 3 Nat
vec3 = [4, 5, 6]

vec4 : Vect 4 Nat
vec4 = [7, 8, 9, 0]

test : String
test = show $ (vec1 ++ vec2) ++ (vec3 ++ vec4)

test2 : String
test2 = show $ (\x => x) 3

f : Char -> String

g : Int -> Char

x : Int

tmp : String
tmp = f (g x)

-- X -> Void : contra
-- MkUninhabited contra

interface CorrectMonoid s (op : s -> s -> s) where
  unit : s
  unitNeutral : (x : s) -> (op unit x = x,  op x unit = x)
  monoidAssociative : (x, y, z : s) ->  op x (op y z) = op (op x y) z

CorrectMonoid Nat Prelude.Types.plus where
  unit = Z
  unitNeutral n = (plusZeroLeftNeutral n, plusZeroRightNeutral n)
  monoidAssociative = plusAssociative

test3 : (s : Functor f) => (tmp : Functor f -> Type) -> x -> {auto _ : tmp s} -> f x
test3 tmp x = ?test3_rhs

record Test where
  constructor MkTest
  t : Nat

testf : Test
testf = MkTest 1

te : Nat
te = testf.t

test4 : String
test4 =
  let
    xs : Vect 4 Nat
    xs = [1,2,3,4]
    y : Nat
    y = 10
    ys : Vect 3 Nat
    ys = [70,80,90]
  in
    show $ xs ++ (y :: ys)

-- Num (Nat -> Nat) where
--   (*) f g x = f x * g x
--   (+) f g x = f x + g x
--   fromInteger x y = fromInteger x * y



test6 : Vect n a -> Vect n b -> Vect n (a, b)
test6 [] ys = []
test6 (x :: xs) (y :: ys) = (x, y) :: test6 xs ys

0 Set : Type

0 Subset : Set -> Set -> Type

0 intersection : Set -> Set -> Set

0 union : Set -> Set -> Set

subsetTransitive : Subset a b -> Subset b c -> Subset a c

intersectionIn : Subset (intersection s t) s

inUnion : Subset x (union x y)

intersectionInUnion : Subset (intersection s1 s2) (union s1 s2)
intersectionInUnion =
	subsetTransitive intersectionIn inUnion


ex1 : String
ex1 =
  let
    xs : List Nat
    xs = [0, 1, 2]
    x : Nat
    x = 10
  in
    show $ x :: xs


----------
ts1 : Vect 3 Nat

ts2 : Vect n Nat -> Vect n Nat


data ManyParamTy : Nat -> Nat -> Nat -> Type -> Type where
    Cons : ManyParamTy l m n ty


----------


subtraction : (x : Nat) -> Fin (S x) -> Nat
subtraction x FZ = x
subtraction (S x) (FS y) = subtraction x y

ex2 : String
ex2 =
  let
    xs : Vect 3 Nat
    xs = [0, 1, 2]
    x : Nat
    x = 10
  in
    show $ x :: xs


ex3 : String
ex3 =
  let
    f : String -> List Char
    f = unpack
    g : Integer -> String
    g = show
  in
    show $ f (g 10)

ex3_1 : String
ex3_1 =
  let
    f : String -> List Char
    f = unpack
    g : Integer -> String
    g = show
  in
    show $ f $ g 10