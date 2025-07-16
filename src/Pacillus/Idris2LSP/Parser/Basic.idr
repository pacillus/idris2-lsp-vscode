module Pacillus.Idris2LSP.Parser.Basic

-- Identifiers to tell which type the Literal is
public export
data LiteralType = IntegerL | DoubleL | CharL | StringL

public export
LiteralTypeOf : LiteralType -> Type
LiteralTypeOf IntegerL = Integer
LiteralTypeOf DoubleL = Double
LiteralTypeOf CharL = Char
LiteralTypeOf StringL = String

public export
data IdentifierType = 
          NameId -- id
        | OperatorId -- +
        | MemberId -- .fun

public export
Eq IdentifierType where
    NameId == NameId = True
    OperatorId == OperatorId = True
    MemberId == MemberId = True
    _ == _ = False

public export
data Identifier = MkIdentifier IdentifierType String

public export
Show Identifier where
    show (MkIdentifier NameId str) = str
    show (MkIdentifier OperatorId str) = str
    show (MkIdentifier MemberId str) = "." ++ str

export
Eq Identifier where
    (MkIdentifier tl strl) == (MkIdentifier tr strr) = tl == tr && strl == strr

public export
data Hole = MkHole String

export
Show Hole where
    show (MkHole id) = id

export
Eq Hole where
    MkHole strl == MkHole strr = strl == strr

public export
data Operator = MkOperator String

public export
data Member = MkMember String

namespace Sugared
    public export
    data ArrowType =
          SingleLine -- ->
        | DoubleLine -- =>

    -- in the data type Sugared, there are two sub type Expression(Expr) and Signature(Sig)
    public export
    data SubSyntaxGroup = Sig | Expr

    public export
    data Sugared : SubSyntaxGroup -> Type where
        -- expressions
        IdentifierTerm : Identifier -> Sugared Expr -- ex) id
        Application : Sugared Expr -> Sugared Expr -> Sugared Expr -- ex) f x
        Arrow : ArrowType -> Sugared Expr -> Sugared Expr -> Sugared Expr -- ex) a -> b A => B
        SignatureArrow : ArrowType -> Identifier -> Sugared Expr -> Sugared Expr -> Sugared Expr -- ex) (x : a) -> B(x) (x : A) => B
        BracketArrow : Identifier -> Sugared Expr -> Sugared Expr -> Sugared Expr -- ex) {x : a} -> B(x)
        AnonymousFunction : Identifier -> Sugared Expr -> Sugared Expr -- \x => e
        Literal : (t : LiteralType) -> LiteralTypeOf t -> Sugared Expr
        Wildcard : Sugared Expr -- _
        HoleTerm : Hole -> Sugared Expr
        UnitSugar : Sugared Expr -- () MkUnit Unit
        PairSugar : Sugared Expr -> Sugared Expr -> Sugared Expr -- (a, b) Pair a b Mkpair a b
        OpInfixSugar : Sugared Expr -> Operator -> Sugared Expr -> Sugared Expr -- 1 + 2
        InfixSugar : Sugared Expr -> Identifier -> Sugared Expr -> Sugared Expr -- 1 'function' 2
        DependentPairSugar : Identifier -> Sugared Expr -> Sugared Expr -> Sugared Expr -- (x : A ** B)
        DependentPairConstructorSugar : Sugared Expr -> Sugared Expr -> Sugared Expr-- (x ** y) MkDPair
        EqualSugar : Sugared Expr -> Sugared Expr -> Sugared Expr -- x = y
        MemberSugar : Sugared Expr -> Member -> Sugared Expr -- x.fst (.fst)
        -- signature
        Signature : Identifier -> Sugared Expr -> Sugared Sig -- x : a

namespace Desugared
    public export
    data BinderType = 
          Pi -- (x : a) -> b
        | Lambda -- \x => e
        | Auto -- (x : a) => b, (auto x : a) -> b
        | Implicit -- {x : a} -> b(x)

    public export
    Eq BinderType where
        Pi == Pi = True
        Lambda == Lambda = True
        Auto == Auto = True
        Implicit == Implicit = True
        _ == _ = False

    public export
    Show BinderType where
        show Pi = "Pi"
        show Lambda = "Lambda"
        show Auto = "Auto"
        show Implicit = "Implicit"

    public export
    data BinderName : Type where
        NamedBinder : Identifier -> BinderName -- \x => 
        AnonymousBinder : BinderName -- \_ =>

    public export
    Show BinderName where
        show (NamedBinder id) = show id
        show AnonymousBinder = "_"

    export
    getBinderName : BinderName -> Identifier
    getBinderName (NamedBinder x) = x
    getBinderName AnonymousBinder = MkIdentifier NameId "_"

    public export
    data DesugaredType = NoHole | WithHole

    public export
    data Desugared : DesugaredType -> Type where
        -- expressions
        Constant : Identifier -> Desugared t
        Index : Identifier -> Nat -> Desugared t
        Application : Desugared t -> Desugared t -> Desugared t
        Binder : BinderType -> BinderName -> Desugared t -> Desugared t -> Desugared t
        Literal : (t : LiteralType) -> LiteralTypeOf t -> Desugared dt
        Wildcard : Desugared NoHole -- _
        HoleTerm : Hole -> Desugared NoHole
        -- below are used internally to derive type
        ImplicitVariable : Identifier -> Nat -> Desugared WithHole --
        Assumption : Identifier -> Nat -> Desugared WithHole  

    public export
    data DesugaredSignature : DesugaredType -> Type where
        MkDSig : Identifier -> Desugared t -> DesugaredSignature t

public export
data ExprSignature : Type where
    MkExprSignature : Desugared WithHole -> Desugared WithHole -> ExprSignature

export
(.type) : ExprSignature -> Desugared WithHole
(MkExprSignature e ty).type = ty

public export
data TypeTree : Type where
    Start : ExprSignature -> TypeTree
    Subgoal : List TypeTree -> ExprSignature -> TypeTree

export
getSubgoal : TypeTree -> ExprSignature
getSubgoal (Start x) = x
getSubgoal (Subgoal xs x) = x