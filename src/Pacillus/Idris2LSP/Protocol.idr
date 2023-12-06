module Pacillus.Idris2LSP.Protocol
{- 
"Right $ MkSignature result_name $ assign applied"

js->idr
"Right $ MkSignature result_name $ assign applied"
idr->js
"0 6 8..."
js->idr
"
Right $ MkSignature result_name $ assign applied
Prelude.Right : b -> Either a b
Pacillus.Idris2LSP.Syntax.SimpleExpr.MkSignature : String -> SimpleExpr -> Signature
...
"
-}