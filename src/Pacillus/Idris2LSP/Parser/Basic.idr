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
        IdentifierTerm : Identifier -> Sugared Expr
        Application : Sugared Expr -> Sugared Expr -> Sugared Expr
        Arrow : ArrowType -> Sugared Expr -> Sugared Expr -> Sugared Expr
        SignatureArrow : ArrowType -> Sugared Sig -> Sugared Expr -> Sugared Expr
        BracketArrow : Sugared Sig -> Sugared Expr -> Sugared Expr
        AnonymousFunction : Identifier -> Sugared Expr -> Sugared Expr
        Literal : (t : LiteralType) -> LiteralTypeOf t -> Sugared Expr
        UnitSugar : Sugared Expr
        PairSugar : Sugared Expr -> Sugared Expr -> Sugared Expr
        OpInfixSugar : Sugared Expr -> Operator -> Sugared Expr -> Sugared Expr
        InfixSugar : Sugared Expr -> Identifier -> Sugared Expr -> Sugared Expr
        DependentPairSugar : Identifier -> Sugared Expr -> Sugared Expr -> Sugared Expr
        EqualSugar : Sugared Expr -> Sugared Expr -> Sugared Expr
        MemberSugar : Sugared Expr -> Member -> Sugared Expr
        DollarSugar : Sugared Expr -> Sugared Expr -> Sugared Expr
        -- signature
        Signature : Identifier -> Sugared Expr -> Sugared Sig



namespace Desugared
    public export
    data BinderType = Pi | Lambda | Auto | Implicit

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
        WildCard : Desugared NoHole
        ImplicitHole : Nat -> Desugared WithHole
    
    public export
    data DesugaredSignature : DesugaredType -> Type where
        MkDSig : IdentifierType -> Identifier -> Desugared t -> DesugaredSignature t

