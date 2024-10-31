#!/bin/bash

cd src

idris2 --cg node Pacillus/Idris2LSP/Lex.idr -o echolex.js -p contrib
idris2 --cg node Pacillus/Idris2LSP/GetType.idr -o echotype.js -p contrib

cp build/exec/echolex.js echolex.js
cp build/exec/echotype.js echotype.js

echo -e -n "exports.Pacillus_Idris2LSP_Lex_lexAndOutput = Pacillus_Idris2LSP_Lex_lexAndOutput\n" >> echolex.js
echo -e -n "exports.Pacillus_Idris2LSP_GetType_process = Pacillus_Idris2LSP_GetType_process\n" >> echotype.js

cd ../

npm install
