# Type Derivation Viewer

## What is Type Derivation Viewer?
It is a tool that can visualize a type derivation of expressions in Idris2.
It shows the process of type derivation in a tree written in text as shown in the example below.

Right now the tool is a prototype and is only implemented as VS Code LSP client.

## How can I run the tool in VS Code?
### Requirements
- [vsce](https://code.visualstudio.com/api/working-with-extensions/publishing-extension#vsce)(Requires Node.js for installation)
- [Idris2LSP Server](https://github.com/idris-community/idris2-lsp)(Includes Idris2 itself)

### Installation
To run the vscode implementation, follow the instruction in the section [Installing the extension](https://github.com/pacillus/idris2-lsp-vscode?tab=readme-ov-file#installing-the-extension) in [idris2-vscode fork](https://github.com/pacillus/idris2-lsp-vscode).

### Executing
Select the expression to derive type (like by click-and-dragging).
Then go to command palette(by pressing `Ctrl + Shift + P` (Windows/Linux) or `Command + Shift + P` (Mac)) and select or type in `Idris: Show the Type Derivation Tree of selection` to show the Type Derivation Tree.

## For other editors
There is no implementation for other editors right now.

## How can I implement this tool in other Editors?
Working in progress...