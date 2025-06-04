module Pacillus.Idris2LSP.Protocol
{- 
"Right $ MkSignature result_name $ assign applied"

js->idr
{"start":{"line":10, "character":4} "text":"Right $ MkSignature result_name $ assign applied"}

idr->js
{\"pos\" : [{"line":"10", character:"4"},{"line":"10", "character":"10"}, {"line":"10", "character""8"},...], syms : ["$"]}

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