import { spawn } from 'child_process';
import {
  workspace,
  ExtensionContext,
  window,
  OutputChannel,
  commands,
  TextEditorEdit,
  TextEditor,
  MarkdownString,
  DecorationRangeBehavior,
  Range,
  WorkspaceEdit,
  Uri,
  Position as VSCodePosition,
  Selection,
} from 'vscode';
import * as vscode from 'vscode';

import {
  CodeAction,
  Hover,
  HoverRequest,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  StreamInfo,
  integer,
} from 'vscode-languageclient/node';

import { Readable } from 'stream';
import * as process from 'process'

import { Pacillus_Idris2LSP_Lex_lexAndOutput } from './lex.js';
import { Pacillus_Idris2LSP_Range_process } from './range.js';
import { start } from 'repl';
import { sign } from 'crypto';

const baseName = 'Idris 2 LSP';

export function activate(context: ExtensionContext) {
  const extensionConfig = workspace.getConfiguration("idris2-lsp");
  const command: string = extensionConfig.get("path") || "";
  const debugChannel = window.createOutputChannel(baseName + ' Server');
  const serverOptions: ServerOptions = () => new Promise<StreamInfo>((resolve, reject) => {
    const serverProcess = spawn(command, [], { cwd: rootPath(), shell: process.platform === 'win32' });
    if (!serverProcess || !serverProcess.pid) {
      return reject(`Launching server using command ${command} failed.`);
    }

    context.subscriptions.push({
      dispose: () => {
        sendExitCommandTo(serverProcess.stdin);
      }
    });

    const stderr = serverProcess.stderr;
    stderr.setEncoding('utf-8');
    stderr.on('data', data => debugChannel.append(data));

    resolve({
      writer: serverProcess.stdin,
      reader: sanitized(serverProcess.stdout, debugChannel),
      detached: true // let us handle the disposal of the server
    });
  });
  const initializationOptions = {
    logSeverity: extensionConfig.get("logSeverity") || "debug",
    logFile: extensionConfig.get("logFile") || "stderr",
    longActionTimeout: extensionConfig.get("longActionTimeout") || 5000,
    maxCodeActionResults: extensionConfig.get("maxCodeActionResults") || 5,
    showImplicits: extensionConfig.get("showImplicits") || false,
    showMachineNames: extensionConfig.get("showMachineNames") || false,
    fullNamespace: extensionConfig.get("fullNamespace") || false,
    briefCompletions: extensionConfig.get("briefCompletions") || false,
  };
  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: 'file', language: 'idris' },
      { scheme: 'file', language: 'markdown', pattern: '**/*.{lidr,idr}.md' },
      { scheme: 'file', language: 'lidr' }
    ],
    initializationOptions: initializationOptions,
  };
  const client = new LanguageClient(
    'idris2-lsp',
    baseName + ' Client',
    serverOptions,
    clientOptions
  );
  client.start();
  context.subscriptions.push({
    dispose: () => {
      client.stop();
    }
  });
  registerCommandHandlersFor(client, context);
}

function registerCommandHandlersFor(client: LanguageClient, context: ExtensionContext) {
  const replDecorationType = window.createTextEditorDecorationType({
    border: '2px inset darkgray',
    borderRadius: '5px',
    after: {
      color: 'darkgray',
    },
    rangeBehavior: DecorationRangeBehavior.ClosedClosed
  });

  context.subscriptions.push(
    commands.registerTextEditorCommand(
      'idris2-lsp.repl.eval',
      (editor: TextEditor, _edit: TextEditorEdit, customCode) => {
        const code: string = customCode || editor.document.getText(editor.selection);
        if (code.length == 0) {
          // clear decorations
          editor.setDecorations(replDecorationType, []);
          return;
        }
        client
          .sendRequest("workspace/executeCommand", { command: "repl", arguments: [code] })
          .then(
            (res) => {
              const code = res as string;
              return {
                hover: new MarkdownString().appendCodeblock(code, 'text'),
                preview: code
              };
            },
            (e) => {
              const error = `${e}`;
              return {
                hover: new MarkdownString().appendText(error),
                preview: error
              };
            }
          )
          .then((res) => {
            console.log(`>${res.preview}<`);
            editor.setDecorations(
              replDecorationType,
              [{
                range: editor.selection,
                hoverMessage: res.hover,
                renderOptions: {
                  after: {
                    contentText: ' => ' + inlineReplPreviewFor(res.preview) + ' ',
                  },
                }
              }]
            );
          });
      }
    )
  );
  context.subscriptions.push(
    commands.registerTextEditorCommand(
      'idris2-lsp.repl.typeat',
      (editor: TextEditor, _edit: TextEditorEdit, customCode) => {
        const code: string = customCode || editor.document.getText(editor.selection);
        const pos = editor.selection.start;
        const ln = pos.line;
        const ch = pos.character;
        if (code.length == 0) {
          // clear decorations
          editor.setDecorations(replDecorationType, []);
          return;
        }
        client
          .sendRequest("workspace/executeCommand", { command: "repl", arguments: [":typeat " + ln + " " + ch + " " + code] })
          .then(
            (res) => {
              const code = res as string;
              return {
                hover: new MarkdownString().appendCodeblock(code, 'idris'),
                preview: code
              };
            },
            (e) => {
              const error = `${e}`;
              return {
                hover: new MarkdownString().appendText(error),
                preview: error
              };
            }
          )
          .then((res) => {
            console.log(`>${res.preview}<`);
            editor.setDecorations(
              replDecorationType,
              [{
                range: editor.selection,
                hoverMessage: res.hover,
                renderOptions: {
                  after: {
                    contentText: ' => ' + inlineReplPreviewFor(res.preview) + ' ',
                  },
                }
              }]
            );
          });
      }
    )
  );

  context.subscriptions.push(
    commands.registerTextEditorCommand(
      'idris2-lsp.pacillus.derivetype',
      async (editor: TextEditor, _edit: TextEditorEdit, customCode) => {
        const SYSTEM_REPLACING_VARIABLE = '__systemTmpHole'
        const document = editor.document
        const code = editor.document.getText();
        const target: string = customCode || editor.document.getText(editor.selection);
        const uri = editor.document.uri
        const pos = editor.selection.start;
        const target_start_line = pos.line;
        const target_start_char = pos.character;

        if (target.length == 0) {
          // clear decorations
          editor.setDecorations(replDecorationType, []);
          return;
        }

        console.log("code:" + target)
        const targetInfo: object = {
          start: {
            line: String(target_start_line),
            character: String(target_start_char)
          },
          text: target
        }

        console.log("sending to idris:" + JSON.stringify(targetInfo))

        const str: string = String(Pacillus_Idris2LSP_Lex_lexAndOutput(JSON.stringify(targetInfo)))
        console.log("informations from idris:" + str)
        const json = JSON.parse(str)
        const token_pos: {
          start: { line: number, character: number }
          end: { line: number, character: number }
        }[] = json.pos.map(
          (x: { start: { line: string, character: string }, end: { line: string, character: string } }) =>
            ({ start: ({ line: parseInt(x.start.line), character: parseInt(x.start.character) }), end: ({ line: parseInt(x.end.line), character: parseInt(x.end.character) }) })
        );
        const ops: object[] = [];
        let syms: string[] = json.syms;

        console.log("syms :", syms);

        for (const sym of syms) {
          console.log("sym :", sym);
          const response = await client.sendRequest("workspace/executeCommand", { command: "repl", arguments: [":doc (" + sym + ")"] })

          console.log("lex response :", response);

          const splitedbyn: string[] = response.toString().split("\n");
          // search for "Fixity Declaration"
          let infopos: integer = 0;
          for (let i = 0; i < splitedbyn.length; i++) {
            if (splitedbyn[i].includes("Fixity Declaration")) {
              infopos = i
            }
          }
          const splitedbyspace: string[] = splitedbyn[infopos].split(" ").filter((x: string) => x !== "");

          const assoc: string = splitedbyspace[2];
          const prec: string = splitedbyspace[5];
          const op: object = { symbol: sym, assoc: assoc, prec: prec };
          ops.push(op);
        }

        const inputobj: object = {
          expr: target,
          ops: ops
        };
        console.log(inputobj);

        const response = Pacillus_Idris2LSP_Range_process(JSON.stringify(inputobj))
        console.log("parse response :", response);
        const output: Tree<TokenRange> = JSON.parse(response);
        console.log("range information : ", output);

        async function askType(selection: TokenRange): Promise<ExpressionSignature> {
          const lines = code.split(/\r?\n/);
          const line_start = token_pos[selection.start].start.line
          const char_start = token_pos[selection.start].start.character
          const line_end = token_pos[selection.end - 1].end.line
          const char_end = token_pos[selection.end - 1].end.character
          let expression = "";
          if (line_start === line_end) {
              expression = lines[line_start].substring(char_start, char_end);
          } else {
              expression += lines[line_start].substring(char_start) + "\n";
              for (let i = line_start + 1; i < line_end; i++) {
                  expression += lines[i] + "\n";
              }
              expression += lines[line_end].substring(0, char_end);
          }
          const replacement = '?' + SYSTEM_REPLACING_VARIABLE
          await editor.edit(editBuilder => {
              const range = new vscode.Range(line_start, char_start, line_end, char_end);
              editBuilder.replace(range, replacement);
          });

          
          const doc = await vscode.workspace.openTextDocument(uri); // 最新の TextDocument を取得
          await doc.save();
          await client.onReady();

          await new Promise(resolve => setTimeout(resolve, 200));

          const response: Hover = await client.sendRequest(HoverRequest.type, {
            textDocument: { uri: "file://" + uri.fsPath}, position: { line: line_start, character: char_start + 1} 
          })

          // --- 元に戻す ---
          await editor.edit(editBuilder => {
              const range = new vscode.Range(
                  line_start, char_start,
                  line_end, char_start + replacement.length // endCol ではなく置換長
              );
              editBuilder.replace(range, expression.split(/\r?\n/).join("\n"));
          });

          await document.save();

          const response_string : string = Array.isArray(response.contents)
                ? response.contents.map(c => (typeof c === 'string' ? c : c.value)).join('\n---\n')
                : typeof response.contents === 'string'
                ? response.contents
                : response.contents.value;
          const response_lines : string[] = response_string.split('\n')
          const signature_line : string = response_lines[response_lines.length - 2]
          const replacemnt_pos = signature_line.indexOf(SYSTEM_REPLACING_VARIABLE);
          const signature = signature_line.slice(replacemnt_pos + (SYSTEM_REPLACING_VARIABLE + ' : ').length);

          return new ExpressionSignature(expression, signature);
        }

        async function askTypesTree(tree: Tree<TokenRange>): Promise<Tree<ExpressionSignature>> {
          if (tree.tag === "atom") {
            const val = await askType(tree.value);
            return { tag: "atom", value: val }
          } else {
              const val = await askType(tree.value);
              const branches = [];

              for(const branch of tree.branches){
                branches.push(await askTypesTree(branch));
              }
              return { tag: "compound", value: val, branches: branches }
          }
        }

        const output_tree = await askTypesTree(output)

        console.log("output_tree : ", output_tree);
        const treeDataProvider = new TypeDerivationTreeDataProvider(output_tree)
        vscode.window.createTreeView('typeTreeView', {
          treeDataProvider: treeDataProvider,
          showCollapseAll: true
        });
        vscode.commands.executeCommand('workbench.view.extension.typeTreeViewContainer');
        vscode.window.showInformationMessage('Tree View を作成しました（エクスプローラーの最下部に表示されます）');
      }
    )
  );

  context.subscriptions.push(
    commands.registerTextEditorCommand(
      'idris2-lsp.refineHole',
      (editor: TextEditor, _edit: TextEditorEdit) => {
        if (editor.document.isDirty) {
          window.showErrorMessage("Unable to refine with unsaved changes");
          return;
        }
        window.showInputBox({
          placeHolder: "Refine with",
        }).then(
          (hint) => {
            if (hint) {
              const range = editor.document.getWordRangeAtPosition(editor.selection.active);
              const params = {
                codeAction: {
                  textDocument: {
                    uri: editor.document.uri.toString(),
                  },
                  range: {
                    start: range.start,
                    end: range.end,
                  },
                  context: {
                    diagnostics: [],
                  },
                },
                hint: hint,
              };
              client
                .sendRequest("workspace/executeCommand", { command: "refineHole", arguments: [params] })
                .then(
                  (res) => {
                    const actions = res as CodeAction[];

                    // Currently, if the server encounters an error while trying to refine,
                    // it just logs the error and responds with an empty list of edits.
                    // If the server is updated to respond with the errors, this generic error message can be removed.
                    if (actions.length === 0) {
                      window.showErrorMessage("Failed to refine");
                    } else {
                      const workspaceEdit = new WorkspaceEdit();

                      for (const action of actions) {
                        if (action.edit) {
                          for (const uri in action.edit.changes) {
                            for (const change of action.edit.changes[uri]) {
                              workspaceEdit.replace(Uri.parse(uri), change.range as Range, change.newText);
                            }
                          }
                        }
                      }

                      workspace.applyEdit(workspaceEdit).then(
                        (success) => {
                          if (!success) {
                            window.showErrorMessage("Failed to apply edit");
                          }
                        },
                        (e) => window.showErrorMessage(`${e}`),
                      );
                    }
                  },
                  (e) => window.showErrorMessage(`${e}`),
                );
            }
          }
        );
      }
    )
  );

  context.subscriptions.push(
    commands.registerTextEditorCommand(
      'idris2-lsp.metavars',
      async (editor: TextEditor, _edit: TextEditorEdit) => {
        try {
          const result = await client.sendRequest("workspace/executeCommand", { command: "metavars" });

          if (!Array.isArray(result) || result.length === 0) {
            window.showInformationMessage('No metavars in context');
            return;
          }

          const items = result.map(metavar => ({
            label: `${metavar.name} : ${metavar.type}`,
            metavar: metavar
          }));

          const selected = await window.showQuickPick(items, {
            placeHolder: 'Select a metavariable to jump to',
          });

          if (selected && selected.metavar.location) {
            const location = selected.metavar.location;
            const uri = Uri.parse(location.uri);
            const position = new VSCodePosition(location.range.start.line, location.range.start.character);
            const vscodePosition = new VSCodePosition(position.line, position.character);
            const selection = new Selection(vscodePosition, vscodePosition);
            const range = new Range(vscodePosition, vscodePosition);
            const doc = await workspace.openTextDocument(uri);
            await window.showTextDocument(doc);
            editor.selection = selection;
            editor.revealRange(range, 1);
          }
        } catch (error) {
          window.showErrorMessage(`Error fetching metavars: ${error}`);
        }
      }
    )
  );
}

function inlineReplPreviewFor(res: string) {
  const maxPreviewLength = 80;
  const lines = res.split(/\r?\n/, 2);
  const firstLine = lines[0];
  const ellipsis = '…';
  if (lines.length > 1) {
    return firstLine.substring(0, maxPreviewLength) + ellipsis;
  }
  return firstLine.length > maxPreviewLength
    ? firstLine.substring(0, maxPreviewLength) + ellipsis
    : firstLine;
}

function sendExitCommandTo(server: NodeJS.WritableStream) {
  const command = '{"jsonrpc":"2.0","method":"exit"}';
  server.write(`Content-Length: ${command.length}\r\n\r\n`);
  server.write(command);
}

/**
 * Returns a new stream with spurious content removed, anything between proper
 * [LSP messages](https://microsoft.github.io/language-server-protocol/specifications/specification-3-14/)
 * is discarded.
 *
 * This is necessary because the Idris 2 core writes error messages directly to stdout.
 *
 * @param source idris2-lsp stdout
 */
function sanitized(source: Readable, debugChannel: OutputChannel): NodeJS.ReadableStream {
  return Readable.from(sanitize(source, debugChannel));
}

async function* sanitize(source: Readable, debugChannel: OutputChannel) {

  let waitingFor = 0;
  let chunks = [];

  for await (const chunk of source) {
    if (waitingFor > 0) {
      // We are already reading a message
      if (chunk.length > waitingFor) {
        const remaining = chunk.subarray(waitingFor);
        chunks.push(remaining);

        const awaited = chunk.subarray(0, waitingFor);
        waitingFor = 0;
        yield awaited;
      }

      waitingFor -= chunk.length;

      yield chunk;
      continue;
    }

    chunks.push(chunk);

    while (chunks.length > 0) {
      const pending = Buffer.concat(chunks);
      const header = findHeader(pending);
      if (header) {
        if (header.begin > 0) {
          debugDiscarded(pending.subarray(0, header.begin));
        }
        const contentLength = header.contentLength;
        const contentEnd = header.end + contentLength;
        const newChunk = pending.subarray(header.begin, contentEnd);
        const headerLength = header.end - header.begin;
        waitingFor = headerLength + contentLength - newChunk.length;
        chunks = waitingFor > 0 ? [] : [pending.subarray(contentEnd)];
        yield newChunk;
      } else {
        // Reuse concat result
        chunks = [pending];
        break;
      }
    }
  }

  function debugDiscarded(discarded: Buffer) {
    debugChannel.appendLine("> STDOUT");
    debugChannel.append(discarded.toString('utf-8'));
    debugChannel.appendLine("< STDOUT");
  }
}

interface ContentHeader {
  begin: number,
  end: number,
  contentLength: number
}

function findHeader(buffer: Buffer): undefined | ContentHeader {
  // Search the buffer for the pattern `Content-Length: \d+\r\n\r\n`
  let searchIndex = 0;
  while (searchIndex < buffer.length) {
    const headerPattern = 'Content-Length: ';
    const separatorPattern = '\r\n\r\n';
    const begin = buffer.indexOf(headerPattern, searchIndex);
    if (begin < 0) {
      break;
    }
    const lengthBegin = begin + headerPattern.length;
    const separatorIndex = buffer.indexOf(separatorPattern, lengthBegin);
    if (separatorIndex > lengthBegin) {
      const lengthBuffer = buffer.subarray(lengthBegin, separatorIndex);
      if (lengthBuffer.every((value, _index, _array) => isDigit(value))) {
        const contentLength = Number.parseInt(lengthBuffer.toString('utf-8'));
        const end = separatorIndex + separatorPattern.length;
        return { begin, end, contentLength };
      }
    }
    searchIndex = lengthBegin;
  }
  return undefined;
}

function isDigit(value: number): boolean {
  return value >= zero && value <= nine;
}

const zero = '0'.charCodeAt(0);

const nine = '9'.charCodeAt(0);

function rootPath(): string | undefined {
  const folders = workspace.workspaceFolders;
  if (!folders || folders.length === 0) {
    return undefined;
  }
  const folder = folders[0];
  if (folder.uri.scheme === 'file') {
    return folder.uri.fsPath;
  }
  return undefined;
}

type Tree<T> =
  | {
    value: T,
    tag: "atom"
  }
  | {
    value: T,
    tag: "compound",
    branches: Tree<T>[]
  }
type TokenRange = { start: integer, end: integer }

class ExpressionSignature {
  constructor(
    public readonly expression: string,
    public readonly type: string
  ) { }

  show(): vscode.TreeItemLabel {
    //return { label: this.expression.concat(" : ").concat(this.type), highlights: [[this.expression.length + 3, this.expression.length + this.type.length + 3]] }
    return { label: this.expression.concat(" : ").concat(this.type) }
  }
}


class TypeDerivationTreeDataProvider implements vscode.TreeDataProvider<Tree<ExpressionSignature>> {
  private _onDidChangeTreeData = new vscode.EventEmitter<Tree<ExpressionSignature> | undefined>();
  readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

  constructor(public readonly tree: Tree<ExpressionSignature>) { }

  getTreeItem(element: Tree<ExpressionSignature>): vscode.TreeItem {
    if (element.tag === "atom") {
      const item = new vscode.TreeItem(element.value.show(), vscode.TreeItemCollapsibleState.None)
      return item;
    } else {
      return new vscode.TreeItem(element.value.show(), vscode.TreeItemCollapsibleState.Collapsed);
    }

  }

  getChildren(element?: Tree<ExpressionSignature>): Thenable<Tree<ExpressionSignature>[]> {
    if (!element) {
      return Promise.resolve([this.tree]);
    }
    if (element.tag === "atom"){
      return Promise.resolve([]);
    } else{
      return Promise.resolve(element.branches);
    }
  }
}