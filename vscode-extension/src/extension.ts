import * as path from 'path';
import { spawn } from 'child_process';
import { commands, window, workspace } from 'vscode';
import type { ExtensionContext, TextDocument, Uri, WorkspaceFolder, OutputChannel } from 'vscode';

import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

// The current map of documents & folders to language servers.
// It may be null to indicate that we are in the process of launching a server,
// in which case don't try to launch another one for that uri
const clients: Map<string, LanguageClient | null> = new Map();

export function activate(context: ExtensionContext) {
  async function startForDocument(document: TextDocument) {
    await activeServer(context, document);
  }

  workspace.onDidOpenTextDocument(startForDocument);
  workspace.textDocuments.forEach(startForDocument);

  workspace.onDidChangeWorkspaceFolders((event) => {
    for (const folder of event.removed) {
      const client = clients.get(folder.uri.toString());
      if (client) {
        clients.delete(folder.uri.toString());
        client.stop();
      }
    }
  });

  const restartCmd = commands.registerCommand('haskell-pyls.commands.restartServer', async () => {
    for (const langClient of clients.values()) {
      await langClient?.stop();
      langClient?.start();
    }
  });
  context.subscriptions.push(restartCmd);
}

async function activeServer(context: ExtensionContext, document: TextDocument) {
  if ((document.languageId !== 'python') || (document.uri.scheme !== 'file')) {
    return;
  }

  const uri = document.uri;
  const folder = workspace.getWorkspaceFolder(uri);

  activateServerForFolder(context, uri, folder);
}

async function activateServerForFolder(context: ExtensionContext, uri: Uri, folder?: WorkspaceFolder) {
  const clientsKey = folder ? folder.uri.toString() : uri.toString();

  if (clients.has(clientsKey)) {
    return;
  }

  // Set the key to null to prevent multiple servers being launched at once
  clients.set(clientsKey, null);

  const langName = 'Haskell PyLS' + (folder ? ` (${folder.name})` : '');
  const outputChannel = window.createOutputChannel(langName);

  const serverPath = context.asAbsolutePath(
    path.join('..', 'build', 'haskell-pyls-exe')
  );
  const args: string[] = [];
  const serverOptions = async function spawnServerProcess() {
    const server = spawn(serverPath, args, {});
    server.stderr.on('data', (stderrChunk: string) => {
      outputChannel.appendLine(`[stderr] ${stderrChunk}`);
    });
    server.stdout.on('data', (stdoutChunk: string) => {
      outputChannel.appendLine(`[stdout] ${stdoutChunk}`);
    });

    return server;
  }

  const pat = folder ? `${folder.uri.fsPath}/**/*` : '**/*';
  const clientOptions: LanguageClientOptions = {
    // Use the document selector to only notify the LSP on files inside the folder
    // path for the specific workspace.
    documentSelector: [
      { scheme: 'file', language: 'python', pattern: pat },
    ],
    diagnosticCollectionName: langName,
    revealOutputChannelOn: RevealOutputChannelOn.Never,
    outputChannel,
    outputChannelName: langName,
    // Launch the server in the directory of the workspace folder.
    workspaceFolder: folder,
  };

  // Create the LSP client.
  const langClient = new LanguageClient(langName, langName, serverOptions, clientOptions);

  // Register ClientCapabilities for stuff like window/progress
  langClient.registerProposedFeatures();

  // Finally start the client and add it to the list of clients.
  langClient.start();
  clients.set(clientsKey, langClient);
}

export async function deactivate() {
  const promises: Array<Thenable<void>> = [];
  for (const client of clients.values()) {
    if (client) {
      promises.push(client.stop());
    }
  }
  await Promise.all(promises);
}
