import Data.String
import Data.Vect

data Keywords = Param1 | Param2 | End

0 argType : (0 _ : Type) -> (key : Keywords) -> Type
argType t Param1 = Char -> (key : Keywords) -> argType t key
argType t Param2 = Nat -> (key : Keywords) -> argType t key
argType t End = t

internalFunction : Char -> Nat -> String
internalFunction c k = singleton c ++ show k

func' : Char -> Nat -> (key : Keywords) -> argType String key
func' c k Param1 = \c' => func' c' k
func' c k Param2 = \k' => func' c k'
func' c k End = internalFunction c k

func : (key : Keywords) -> argType String key
func = func' '#' 0

test1 : String
test1 = func Param2 10 Param1 '*' End

test2 : String
test2 = func Param1 '*' Param2 10 End

test3 : String
test3 = func End

test0 : String
test0 = func Param1 ?param1hole Param2 ?param2hole Param1 ?anotherhole End

-- yay :)

-- now it's time for generalization~♪ (I mean why not)
-- to handle n-arg functions, we will be using Fun Type in Data.Fun
-- and to index where the keyword is indexing we will be using Maybe (Fin n)
--   where Nothing means end of arguments and Just n means its the n-th index
-- to define the relation between the index and keyword
--   `{0 keyword : Type} -> keyword -> Maybe (Fin n)` is used

public export
0 Fun : (0 _ : Vect n Type) -> (0 _ :Type) -> Type
Fun [] r = r
Fun (t::ts) r = t -> Fun ts r

namespace KeywordArg
    0 argType :
        (0 argty : Vect n Type) ->
        (0 _ : Type) -> 
        (keywords -> Maybe (Fin n)) ->
        keywords -> 
        Type
    argType argument_type return_type indexer keyword with (indexer keyword)
      argType argument_type return_type indexer keyword | Nothing = return_type
      argType argument_type return_type indexer keyword | (Just k) 
        = index k argument_type -> (key : keywords) -> argType argument_type return_type indexer key

    assignArg :
        {n : Nat} ->
        (0 argty : Vect n Type) ->
        (0 t : Type) -> 
        (k : Fin n) ->
        (f : Fun argty t) ->
        (x : index k argty) ->
        Fun argty t
    assignArg [] t k f x impossible -- k : Fin 0
    assignArg (a :: xs) t FZ f x = \_ => f x
    assignArg (a :: xs) t (FS i) f x = \y => assignArg xs t i (f y) x

    embed : {n : Nat} -> (0 argty : Vect n Type) -> (0 retty : a -> Type) -> ((x : a) -> Fun argty (retty x)) -> Fun argty ((x : a) -> retty x)
    embed [] _ f = f
    embed (b :: as) _ f = \x => embed as _ $ \y => f y x

    backEnd : 
        {n : Nat} ->
        (0 argty : Vect n Type) ->
        (0 t : Type) -> 
        (indexer : keywords -> Maybe (Fin n)) -> 
        (f : Fun argty t) ->
        (key : keywords) -> 
        Fun argty (argType argty t indexer key)
    backEnd argty t indexer f key with (indexer key) proof prf
      backEnd argty t indexer f key | Nothing = f
      backEnd argty t indexer f key | (Just i) 
        = embed argty _ (\arg => assignArg argty ((key : keywords) -> argType argty t indexer key) i (embed argty (argType argty t indexer) $ backEnd argty t indexer f) arg)

    -- end of definition!
    -- now for giving an example

    f : Nat -> Char -> Int -> String
    f n c i = show n ++ singleton c ++ show i

    data ArgExample = A1 | A2 | A3 | End

    argty : Vect 3 Type
    argty = [Nat, Char, Int]

    indexer : ArgExample -> Maybe (Fin 3)
    indexer A1 = Just 0
    indexer A2 = Just 1
    indexer A3 = Just 2
    indexer End = Nothing

    0 FrontEndType : Type
    FrontEndType = (key : ArgExample) -> argType argty String indexer key

    f' : FrontEndType
    f' key = backEnd argty String ?arg2 ?arg3 ?arg4 ?arg5 ?arg6 ?arg7

    frontEnd : FrontEndType
    frontEnd key = backEnd argty String indexer f key 0 '*' (-10)

    public export
    test4 : String
    test4 = frontEnd End

    public export
    test5 : String
    test5 = frontEnd A2 '+' End

    public export
    test6 : String
    test6 = frontEnd A2 '%' A3 (-1000) A1 (S Z) End

    -- holes work as just as fine
    ex1 : String
    ex1 = frontEnd A3 ?a3 A2 ?a2 A1 ?a1 End