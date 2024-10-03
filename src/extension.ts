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
} from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  StreamInfo,
  integer,
} from 'vscode-languageclient/node';

import { Readable } from 'stream';
import * as process from 'process'

import {Pacillus_Idris2LSP_Lex_lexAndOutput} from './echolex.js';
import {Pacillus_Idris2LSP_GetType_process} from './echotype.js';

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
  let initializationOptions = {
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
      'idris2-lsp.repl.typeat',
      (editor: TextEditor, _edit: TextEditorEdit, customCode) => {
        const code: string = customCode || editor.document.getText(editor.selection);
        const uri = editor.document.uri.fsPath
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
      'idris2-lsp.pacillus.test',
      (editor: TextEditor, _edit: TextEditorEdit, customCode) => {
        const code: string = customCode || editor.document.getText(editor.selection);
        const uri = editor.document.uri.fsPath
        const pos = editor.selection.start;
        const ln = pos.line;
        const ch = pos.character;

        if (code.length == 0) {
          // clear decorations
          editor.setDecorations(replDecorationType, []);
          return;
        }



        var str : string = String(Pacillus_Idris2LSP_Lex_lexAndOutput(code))
        var json = JSON.parse(str)
        var tops : number[] = json.pos.map((x : string) => parseInt(x));
        var sigs : string[] = new Array();
        var ops : object[] = new Array();
        var syms :string[] = json.syms;

        
        // magic code that makes all well
        // made with recursion
        const f = (tops : number [], i: number, max: number) => {
          if (i < max) {
            client
            .sendRequest('textDocument/hover', {textDocument: {uri: "file://" + uri}, position: {line: ln, character: ch + tops[i]}})
            .then((my_res: any) => {
              console.log(my_res);
              const splited: any[] = my_res.contents.value.split("\n");
              var sig = String(splited[splited.length - 2].trim())
              sigs.push(sig);
              var str : string = String(Pacillus_Idris2LSP_Lex_lexAndOutput(sig));
              console.log(str);
              var json = JSON.parse(str);
              console.log(json);
              console.log(json.syms);
              syms = syms.concat(json.syms);
              f(tops, i + 1, max);
            })
          } else {



            const g = (syms : string[], i: number, max: number) => {
              if (i < max) {
                client
                .sendRequest("workspace/executeCommand", { command: "repl", arguments: [":doc (" + syms[i] + ")"]})
                .then((my_res: any) => {
                  // console.log(my_res);
                  const splitedbyn: string[] = my_res.toString().split("\n");
                  // search for "Fixity Declaration"
                  var infopos : integer = 0;
                  for (let i = 0; i < splitedbyn.length; i++){
                    if (splitedbyn[i].includes("Fixity Declaration")){
                      infopos = i
                    }
                  }
                  const splitedbyspace: string[] = splitedbyn[infopos].split(" ").filter((x : string) => x !== "");
                  // console.log(splitedbyspace);
                  var assoc : string = splitedbyspace[2];
                  // console.log(assoc);
                  var prec : string = splitedbyspace[5];
                  // console.log(prec);
                  var op : object = {symbol : syms[i],assoc : assoc, prec : prec};
                  ops.push(op);
                  g(syms, i + 1, max);
                })
              } else {
                var inputobj : object = {
                  expr : code,
                  ops : ops,
                  sigs : sigs
                };
                console.log(inputobj);
                console.log(JSON.stringify(inputobj));
                var output = Pacillus_Idris2LSP_GetType_process(JSON.stringify(inputobj));
                  editor.setDecorations(
                    replDecorationType,
                    [{
                      range: editor.selection,
                      hoverMessage: new MarkdownString().appendCodeblock(output, 'idris')
                      // renderOptions: {
                      //   after: {
                      //     contentText: ' => ' + inlineReplPreviewFor(res.preview) + ' ',
                      //   },
                      // }
                    }]
                  );
              }
            };
            console.log(syms);
            g(syms, 0, syms.length);
          }
        };
        // console.log(tops);
        f(tops, 0, tops.length);

        
        // client
        //   .sendRequest("workspace/executeCommand", { command: "repl", arguments: [code] })
        //   .then(
        //     (res) => {
        //       const code = res as string;
        //       return {
        //         hover: new MarkdownString().appendCodeblock(sigs.toString(), 'idris'),
        //         preview: sigs.toString()
        //       };
        //     },
        //     (e) => {
        //       const error = `${e}`;
        //       return {
        //         hover: new MarkdownString().appendText(error),
        //         preview: error
        //       };
        //     }
        //   )
        //   .then((res) => {
        //     console.log(`>${res.preview}<`);
        //     editor.setDecorations(
        //       replDecorationType,
        //       [{
        //         range: editor.selection,
        //         hoverMessage: res.hover,
        //         renderOptions: {
        //           after: {
        //             contentText: ' => ' + inlineReplPreviewFor(res.preview) + ' ',
        //           },
        //         }
        //       }]
        //     );
        //   });
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
