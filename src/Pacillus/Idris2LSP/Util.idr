module Pacillus.Idris2LSP.Util

export
findFirst : String -> (a -> Either String b) -> List a -> Either String b
findFirst errtxt f [] = Left errtxt
findFirst errtxt f (x :: xs) = either (const $ findFirst errtxt f xs) Right (f x)