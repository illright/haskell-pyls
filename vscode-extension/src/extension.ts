import * as path from "path";
import { workspace, ExtensionContext } from "vscode";

import {
	Executable,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  let server = context.asAbsolutePath(
    path.join("..", "build", "haskell-pyls-exe")
  );

  let serverOptions: ServerOptions = { command: server };
 
  let clientOptions: LanguageClientOptions = {
	documentSelector: [{ scheme: "file", language: "python" }]
  };

  // Create the language client and start the client.
  client = new LanguageClient(
    "haskell-pyls",
    "Haskell PyLS",
    serverOptions,
    clientOptions
  );

  // Start the client. This will also launch the server
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
