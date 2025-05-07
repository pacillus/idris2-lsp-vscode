module Pacillus.Idris2LSP.TypeTree.Output

import Pacillus.Idris2LSP.Parser.Basic
-- import Pacillus.Idris2LSP.TypeTree.TypeTree
import Data.String

namespace Identifier
    public export
    output : Identifier -> String
    output (MkIdentifier NameId str) = str
    output (MkIdentifier OperatorId str) = "(\{str})"
    output (MkIdentifier MemberId str) = "." ++ str

namespace Desugared
    output' : Nat -> Desugared WithHole -> String
    output' k (Constant id) = output id
    output' k (Index id _) = show id
    output' k (Application (Application (Constant (MkIdentifier OperatorId w)) z) y) = "(\{output' k z} \{w} \{output' k y})"
    output' k (Application (Constant (MkIdentifier MemberId w)) y) = "(\{output' k y}.\{show w})"
    output' k (Application x y) = "(\{output' k x} \{output' k y})"
    output' k (Binder Pi (NamedBinder id) ty e) = "((\{show id} : \{output' k ty}) -> \{output' k e})"
    output' k (Binder Pi AnonymousBinder ty e) = "(\{output' k ty} -> \{output' k e})"
    output' k (Binder Lambda id ty e) = "\\\{show id} : (\{output' k ty}) => (\{output' k e})"
    output' k (Binder Auto (NamedBinder id) ty e) = "(\{show id} : (\{output' k ty})) => (\{output' k e})"
    output' k (Binder Auto AnonymousBinder ty e) = "(\{output' k ty}) => (\{output' k e})"
    output' k (Binder Implicit (NamedBinder id) ty e) = "(\{show id} : (\{output' k ty})) -> (\{output' k e})"
    output' k (Binder Implicit AnonymousBinder ty e) = "(_ : \{output' k ty}) -> (\{output' k e})"
    output' _ (Literal IntegerL x) = show x
    output' _ (Literal DoubleL x) = show x
    output' _ (Literal CharL x) = show x
    output' _ (Literal StringL x) = show x
    output' _ (ImplicitHole id _) = "?" ++ show id

    export
    output : Desugared WithHole -> String
    output x = output' 0 x

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
    show (ImplicitHole x k) = "(Hole : \{show k}(\{show x}))"

namespace ExprSignature
    export
    output : ExprSignature -> String
    output (MkExprSignature x y) = "\{output x} : \{output y}"

namespace TypeTree
    export
    output : TypeTree -> String
    output (Start sig) = output sig
    output (Subgoal xs x) =
      let
        pres = map ((++) "| ") $ foldl (++) [] $ map (lines . output) xs
      in
        unlines $ output x :: "----------" :: pres
