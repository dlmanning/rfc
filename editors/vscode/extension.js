const vscode = require('vscode');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client;

class RplDebugAdapterDescriptorFactory {
    createDebugAdapterDescriptor(_session, _executable) {
        const config = vscode.workspace.getConfiguration('rpl');
        const debuggerPath = config.get('debugger.path') || 'rpl-dap';
        return new vscode.DebugAdapterExecutable(debuggerPath);
    }
}

class RplCodeLensProvider {
    provideCodeLenses(document, _token) {
        // Add Run/Debug lens at the first non-empty line
        const firstLine = this.findFirstCodeLine(document);
        if (firstLine === null) return [];

        const range = new vscode.Range(firstLine, 0, firstLine, 0);

        return [
            new vscode.CodeLens(range, {
                title: '▶ Run',
                command: 'rpl.run',
                arguments: [document.uri]
            }),
            new vscode.CodeLens(range, {
                title: 'Debug',
                command: 'rpl.debug',
                arguments: [document.uri]
            })
        ];
    }

    findFirstCodeLine(document) {
        for (let i = 0; i < document.lineCount; i++) {
            const line = document.lineAt(i).text.trim();
            // Skip empty lines and comments
            if (line && !line.startsWith('@')) {
                return i;
            }
        }
        return null;
    }
}

function activate(context) {
    const config = vscode.workspace.getConfiguration('rpl');
    const serverPath = config.get('server.path') || 'rpl-lsp';
    const verboseHover = config.get('hover.verbose') || false;

    // Start the language server
    client = new LanguageClient(
        'rpl',
        'RPL Language Server',
        {
            run: { command: serverPath, transport: TransportKind.stdio },
            debug: { command: serverPath, transport: TransportKind.stdio }
        },
        {
            documentSelector: [{ scheme: 'file', language: 'rpl' }],
            initializationOptions: { verboseHover }
        }
    );

    client.start();

    // Register the debug adapter
    const factory = new RplDebugAdapterDescriptorFactory();
    context.subscriptions.push(
        vscode.debug.registerDebugAdapterDescriptorFactory('rpl', factory)
    );

    // Register CodeLens provider
    context.subscriptions.push(
        vscode.languages.registerCodeLensProvider(
            { language: 'rpl', scheme: 'file' },
            new RplCodeLensProvider()
        )
    );

    // Register Run command
    context.subscriptions.push(
        vscode.commands.registerCommand('rpl.run', async (uri) => {
            const filePath = uri?.fsPath || vscode.window.activeTextEditor?.document.uri.fsPath;
            if (!filePath) {
                vscode.window.showErrorMessage('No RPL file to run');
                return;
            }
            // Run without debugging
            const terminal = vscode.window.createTerminal('RPL');
            terminal.show();
            terminal.sendText(`rpl "${filePath}"`);
        })
    );

    // Register Debug command
    context.subscriptions.push(
        vscode.commands.registerCommand('rpl.debug', async (uri) => {
            const filePath = uri?.fsPath || vscode.window.activeTextEditor?.document.uri.fsPath;
            if (!filePath) {
                vscode.window.showErrorMessage('No RPL file to debug');
                return;
            }
            // Start debugging
            await vscode.debug.startDebugging(undefined, {
                type: 'rpl',
                request: 'launch',
                name: 'Debug RPL',
                program: filePath,
                stopOnEntry: true
            });
        })
    );
}

function deactivate() {
    return client?.stop();
}

module.exports = { activate, deactivate };
