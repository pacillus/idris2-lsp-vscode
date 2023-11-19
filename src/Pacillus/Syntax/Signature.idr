module Pacillus.Syntax.Signature

-- ## Tokens ##
-- <STColon> ::= :
-- <STSingleArrow> ::= ->
-- <STDoubleArrow> ::= =>
-- <STIdentifier> ::= SimpleExpr
-- <STUnit> ::= ()
-- <STLParen> ::= (
-- <STRParen> ::= )
-- <STComma> ::= ,
-- <STEqual> ::= =

-- <Signature> ::= <Name> <STColon> <TypeExpr>
-- <TypeExpr> ::=
--     <Factor>
--   | <Factor> <STSingleArrow> <TypeExpr>
--   | <Factor> <STDoubleArrow> <TypeExpr>

-- <Name> ::= <STIdentifier>

-- <TNFactor> ::= <STLParen> <TypeName> <STRParen>

-- <TypeName> ::=
--     <Name> <TypeName>
--   | <Name>
--   | <TNFactor> <TypeName>
--   | <TNFactor>

-- <Pair> ::= <TypeName> <STColon> <TypeName>

-- <Factor> ::=
--     <Name> <STEqual> <Name>
--   | <TNFactor> <TypeName>
--   | <STLParen> <TypeName> <STRParen>
--   | <STLParen> <Signature> <STRParen>
--   | <STLParen> <TypeExpr> <STRParen>
--   | <STLParen> <Pair> <STRParen>
--   | <TypeName>
--   | <STUnit>
