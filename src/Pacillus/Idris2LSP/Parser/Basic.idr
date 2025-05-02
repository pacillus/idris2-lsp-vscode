module Pacillus.Idris2LSP.Parser.Basic

public export
data LiteralType = IntegerL | DoubleL | CharL | StringL

public export
LiteralTypeOf : LiteralType -> Type
LiteralTypeOf IntegerL = Integer
LiteralTypeOf DoubleL = Double
LiteralTypeOf CharL = Char
LiteralTypeOf StringL = String

public export
data Identifier = MkIdentifier String

public export
Show Identifier where
    show (MkIdentifier str) = str

export
Eq Identifier where
    (MkIdentifier strl) == (MkIdentifier strr) = strl == strr

public export
data Operator = MkOperator String

public export
data Member = MkMember String

namespace Sugared
    public export
    data ArrowType = SingleLine | DoubleLine

    public export
    data SubSyntaxGroup = Sig | Expr

    public export
    data Sugared : SubSyntaxGroup -> Type where
        -- expressions
        IdentifierTerm : Identifier -> Sugared Expr -- ex) id
        Application : Sugared Expr -> Sugared Expr -> Sugared Expr -- ex) f x
        Arrow : ArrowType -> Sugared Expr -> Sugared Expr -> Sugared Expr -- ex) a -> b A => B
        SignatureArrow : ArrowType -> Sugared Sig -> Sugared Expr -> Sugared Expr -- ex) (x : a) -> B(x) (x : A) => B
        BracketArrow : Sugared Sig -> Sugared Expr -> Sugared Expr -- ex) {x : a} -> B(x)
        AnonymousFunction : Identifier -> Sugared Expr -> Sugared Expr -- \x => e
        Literal : (t : LiteralType) -> LiteralTypeOf t -> Sugared Expr 
        -- _
        UnitSugar : Sugared Expr -- () MkUnit Unit
        PairSugar : Sugared Expr -> Sugared Expr -> Sugared Expr -- (a, b) Pair a b Mkpair a b
        OpInfixSugar : Sugared Expr -> Operator -> Sugared Expr -> Sugared Expr -- 1 + 2
        InfixSugar : Sugared Expr -> Identifier -> Sugared Expr -> Sugared Expr -- 1 'function' 2
        DependentPairSugar : Identifier -> Sugared Expr -> Sugared Expr -> Sugared Expr -- (x : A ** B)
        -- (x ** y) MkDPair
        EqualSugar : Sugared Expr -> Sugared Expr -> Sugared Expr -- x = y
        MemberSugar : Sugared Expr -> Member -> Sugared Expr -- x.fst (.fst)
        DollarSugar : Sugared Expr -> Sugared Expr -> Sugared Expr -- a $ b 
        -- signature
        Signature : Identifier -> Sugared Expr -> Sugared Sig -- x : a



namespace Desugared
    public export
    data BinderType = Pi | Lambda | Auto | Implicit
    -- (x : a) -> b
    -- \x => e
    -- (x : a) => b
    -- {x : a} -> b(x)
    -- a -> b

    public export
    Eq BinderType where
        Pi == Pi = True
        Lambda == Lambda = True
        Auto == Auto = True
        Implicit == Implicit = True
        _ == _ = False

    public export
    data IdentifierType = NameId | OperatorId | MemberId | InfixId

    public export
    sameIdGroup : IdentifierType -> IdentifierType -> Bool
    sameIdGroup NameId NameId = True
    sameIdGroup OperatorId OperatorId = True
    sameIdGroup MemberId MemberId = True
    sameIdGroup InfixId InfixId = True
    sameIdGroup NameId InfixId = True
    sameIdGroup InfixId NameId = True
    sameIdGroup _ _ = False

    public export
    data BinderName : Type where
        NamedBinder : Identifier -> BinderName
        AnonymousBinder : BinderName

    export
    getBinderName : BinderName -> Identifier
    getBinderName (NamedBinder x) = x
    getBinderName AnonymousBinder = MkIdentifier "_"

    public export
    data DesugaredType = NoHole | WithHole

    public export
    data Desugared : DesugaredType -> Type where
        -- expressions
        Constant : IdentifierType -> Identifier -> Desugared t
        Index : Nat -> Desugared t
        Application : Desugared t -> Desugared t -> Desugared t
        Binder : BinderType -> BinderName -> Desugared t -> Desugared t -> Desugared t
        Literal : (t : LiteralType) -> LiteralTypeOf t -> Desugared dt
        WildCard : Desugared NoHole -- _
        ImplicitHole : Nat -> Desugared WithHole -- 
    
    public export
    data DesugaredSignature : DesugaredType -> Type where
        MkDSig : IdentifierType -> Identifier -> Desugared t -> DesugaredSignature t

