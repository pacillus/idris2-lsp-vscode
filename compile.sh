#!/bin/bash

cd src

idris2 --cg node Pacillus/Idris2LSP/Lex.idr -o lex.js -p contrib
idris2 --cg node Pacillus/Idris2LSP/Range.idr -o range.js -p contrib

cp build/exec/lex.js lex.js
cp build/exec/range.js range.js

echo -e -n "exports.Pacillus_Idris2LSP_Lex_lexAndOutput = Pacillus_Idris2LSP_Lex_lexAndOutput\n" >> lex.js
echo -e -n "exports.Pacillus_Idris2LSP_Range_process = Pacillus_Idris2LSP_Range_process\n" >> range.js

cd ../

npm install
