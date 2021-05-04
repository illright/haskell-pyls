import * as path from 'path';
import { workspace, commands } from "vscode";
import type { ExtensionContext } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const serverPath = context.asAbsolutePath(
    path.join('..', 'build', 'haskell-pyls-exe')
  );
  let serverOptions: ServerOptions = {
    run: { command: serverPath, transport: TransportKind.stdio },
    debug: { command: serverPath, transport: TransportKind.stdio },
  };

  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: 'file', language: 'python' }],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher('**/*.py')
    },
    diagnosticCollectionName: 'diag'
  };

  client = new LanguageClient(
    'haskell-pyls',
    'Haskell PyLS',
    serverOptions,
    clientOptions
  );

  const restartCmd = commands.registerCommand('haskell-pyls.commands.restartServer', async () => {
    await client.stop();
    client.start();
  });
  context.subscriptions.push(restartCmd);

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
