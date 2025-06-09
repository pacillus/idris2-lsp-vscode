# Idris 2 Language Server Extension TyDeViewer fork

A [TyDeViewer](https://github.com/pacillus/TypeDerivationViewer) implementation for VS Code Extension.

Basic information of the upstream is [here](https://github.com/bamboo/idris2-lsp-vscode)

## Requirements

- [vsce](https://code.visualstudio.com/api/working-with-extensions/publishing-extension#vsce)(Requires Node.js for installation)
- [Idris2LSP Server](https://github.com/idris-community/idris2-lsp)(Includes Idris2 itself)

## Installing the extension

~~The `idris2-lsp` extension can be installed from the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=bamboo.idris2-lsp).~~

It can also be built and installed locally from the checkout directory with:

    $ vsce package
    $ code --install-extension idris2-lsp-${version}.vsix

## How to run the TyDeViewer command
Select the expression to derive type (like by click-and-dragging).
Then go to command palette(by pressing `Ctrl + Shift + P` (Windows/Linux) or `Command + Shift + P` (Mac)) and select or type in `Idris: Show the Type Derivation Tree of selection` to show the Type Derivation Tree.

## Configuring the extension

To configure the command used to start the Idris language server, `idris2-lsp` by default, go to `Settings` and search for `idris2`.

## Debugging the extension

- Run `bash compile.sh` in this folder
- Open VS Code on this folder
- Press `Ctrl+Shift+D` / `Cmd+Shift+D` to reveal the everything Debug viewlet
- Select `Launch Client` from the drop down
- Run the launch config
