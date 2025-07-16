module Pacillus.Idris2LSP.TypeTree.Output

import Pacillus.Idris2LSP.Parser.Basic
import Data.String
import Language.JSON

namespace Identifier
    public export
    output : Identifier -> String
    output (MkIdentifier NameId str) = str
    output (MkIdentifier OperatorId str) = "(\{str})"
    output (MkIdentifier MemberId str) = ".\{str}"

data SyntaxLevel = Term | Application | Operator | Arrows 

Eq SyntaxLevel where
 Term == Term = True
 Application == Application = True
 Operator == Operator = True
 Arrows == Arrows = True
 _ == _ = False

Ord SyntaxLevel where
    compare Term Term = EQ
    compare Term r = LT
    compare Application Term = GT
    compare Application Application = EQ
    compare Application r = LT
    compare Operator Arrows = LT
    compare Operator Operator = EQ
    compare Operator r = GT
    compare Arrows Arrows = EQ
    compare Arrows y = GT

withParen : Bool -> String -> String
withParen False str = str
withParen True str = "(\{str})"

namespace Desugared
    output' : SyntaxLevel -> Desugared WithHole -> String
    output' _ (Constant id) = output id
    output' _ (Index id _) = show id
    output' level (Application (Application (Constant (MkIdentifier OperatorId "===")) z) y) = 
        withParen (level < Operator) ("\{output' Application z} = \{output' Application y}")
    output' level (Application (Application (Constant (MkIdentifier OperatorId w)) z) y) = 
        withParen (level < Operator) (output' Application z ++ " " ++ w ++ " " ++ output' Application y)
    output' level (Application (Constant (MkIdentifier MemberId w)) y) = 
        withParen (level < Term) "\{output' Term y}.\{w}"
    output' level (Application x@(Application (Constant str) z) y@(Binder Lambda w v s)) with (str == MkIdentifier NameId "DPair")
      output' level (Application x@(Application (Constant str) z) y@(Binder Lambda w v s)) | False = 
        withParen (level < Application) "\{output' Application x} \{output' Term y}"
      output' level (Application x@(Application (Constant _) z) y@(Binder Lambda w _ s)) | True =
        "(\{show w} : \{output' Arrows z} ** \{output' Arrows s})"
    output' level (Application x@(Application (Constant str) z) y) with (str)
      output' level (Application x@(Application (Constant _) z) y) | MkIdentifier NameId "MkDPair" =
        "(\{output' Arrows z} ** \{output' Arrows y})"
      output' level (Application x@(Application (Constant str) z) y) | MkIdentifier NameId "Pair" = 
        "(\{output' Arrows z}, \{output' Arrows y})"
      output' level (Application x@(Application (Constant str) z) y) | id = 
        withParen (level < Application) "\{output' Application x} \{output' Term y}"
    output' level (Application (Constant $ MkIdentifier NameId "fromInteger") (Literal IntegerL n)) = 
        "\{show n}"
    output' level (Application (Constant $ MkIdentifier NameId "fromDouble") (Literal DoubleL n)) = 
        "\{show n}"
    output' level (Application (Constant $ MkIdentifier NameId "fromChar") (Literal CharL n)) = 
        "\{show n}"
    output' level (Application (Constant $ MkIdentifier NameId "fromString") (Literal StringL n)) = 
        "\{show n}"
    output' level (Application x y) = 
        withParen (level < Application) "\{output' Application x} \{output' Term y}"
    output' level (Binder Pi (NamedBinder id) ty e) = 
        withParen (level < Arrows) "(\{show id} : \{output' Arrows ty}) -> \{output' Arrows e}"
    output' level (Binder Pi AnonymousBinder ty e) = 
        withParen (level < Arrows) "\{output' Operator ty} -> \{output' Arrows e}"
    output' level (Binder Lambda id ty@(ImplicitVariable (MkIdentifier x str) k) e) with (str == "_")
      output' level (Binder Lambda id ty@(ImplicitVariable (MkIdentifier x str) k) e) | False =
        withParen (level < Arrows) "\\\{show id} : \{output' Arrows ty} => \{output' Arrows e}"
      output' level (Binder Lambda id ty@(ImplicitVariable (MkIdentifier x str) k) e) | True = 
        withParen (level < Arrows) "\\\{show id} => \{output' Arrows e}"
    output' level (Binder Lambda id ty e) =
        withParen (level < Arrows) "\\\{show id} : \{output' Arrows ty} => \{output' Arrows e}"
    output' level (Binder Auto (NamedBinder id) ty e) = 
        withParen (level < Arrows) "(\{show id} : (\{output' Arrows ty})) => \{output' Arrows e}"
    output' level (Binder Auto AnonymousBinder ty e) = 
        withParen (level < Arrows) "\{output' Operator ty} => \{output' Arrows e}"
    output' level (Binder Implicit (NamedBinder id) (ImplicitVariable _ _) e) = 
        withParen (level < Arrows) "\{show id} -> \{output' Arrows e}"
    output' level (Binder Implicit (NamedBinder id) ty e) = 
        withParen (level < Arrows) "(\{show id} : \{output' Arrows ty}) -> \{output' Arrows e}"
    output' level (Binder Implicit AnonymousBinder ty e) = 
        withParen (level < Arrows) "(_ : \{output' Arrows ty}) -> (\{output' Arrows e})"
    output' _ (Literal IntegerL x) = show x
    output' _ (Literal DoubleL x) = show x
    output' _ (Literal CharL x) = show x
    output' _ (Literal StringL x) = show x
    output' _ (ImplicitVariable id _) = show id -- "?" ++ show id --
    output' _ (Assumption id n) = show id -- "#" ++ show id

    export
    output : Desugared WithHole -> String
    output x = output' Arrows x

public export
Show (Desugared WithHole) where
    show (Constant x) = show x
    show (Index x k) = "(Index : \{show k}(\{show x}))"
    show (Application x y) = "(\{show x} \{show y})"
    show (Binder x y z w) = "(Binder \{show x} (\{show y} : \{show z}). \{show w})"
    show (Literal IntegerL x) = show x
    show (Literal DoubleL x) = show x
    show (Literal CharL x) = show x
    show (Literal StringL x) = show x
    show (ImplicitVariable x k) = "(ImplicitVar : \{show k}(\{show x}))"
    show (Assumption x k) = "(Assumption : \{show k}(\{show x}))"

namespace ExprSignature
    export
    output : ExprSignature -> String
    output (MkExprSignature x y) = "\{output x} : \{output y}"

namespace TypeTree
    output' : TypeTree -> JSON
    output' (Start sig) = JString $ output sig
    output' (Subgoal xs x) =
        JObject $ ("Conclusion", JString $ output x) :: ("Premises", JArray $ map output' xs) :: []

    export
    output : TypeTree -> String
    output tree = show $ output' tree
