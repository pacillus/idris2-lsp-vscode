module Pacillus.Idris2LSP.Protocol
{- 
"Right $ MkSignature result_name $ assign applied"

js->idr
"Right $ MkSignature result_name $ assign applied"

idr->js
"{\"pos\" : [\"0\",\"6\",\"8\",...], syms : [\"$\"]}"

js->idr
"
{
expr : \"Right $ MkSignature result_name $ assign applied\"
ops : [{"symbol" : "$", "prec" : "0", "assoc" : "infixr"}, ...}]
sigs : [
\"Prelude.Right : b -> Either a b\",
\"Pacillus.Idris2LSP.Syntax.SimpleExpr.MkSignature : String -> SimpleExpr -> Signature\",
...
]
}
"
only allows "infixr" "infixl" "infix" on root.ops.assoc

idr->js
"
| |....
| ----------
| ($) Right : ...
----------
Right $ MkSignature result_name $ assign applied : .....
"
-}