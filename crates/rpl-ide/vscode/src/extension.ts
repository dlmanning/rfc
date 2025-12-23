import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs';

import type { Root } from './rpl-ide/rpl_ide';
import type { TokenType, SymbolKind } from './rpl-ide/interfaces/rpl-ide-types';

// ============================================================================
// Type Mappings (VS Code <-> WIT)
// ============================================================================

const severityMap: Record<string, vscode.DiagnosticSeverity> = {
    'error': vscode.DiagnosticSeverity.Error,
    'warning': vscode.DiagnosticSeverity.Warning,
    'hint': vscode.DiagnosticSeverity.Hint,
};

const witToVscode: Record<TokenType, string> = {
    'comment': 'comment',
    'keyword': 'keyword',
    'number': 'number',
    'string-literal': 'string',
    'operator': 'operator',
    'function': 'function',
    'variable': 'variable',
    'parameter': 'parameter',
    'type-name': 'type',
    'property': 'property',
};

const tokenTypes = Object.values(witToVscode);
const tokenModifiers = ['declaration', 'definition', 'readonly'];
const legend = new vscode.SemanticTokensLegend(tokenTypes, tokenModifiers);

const symbolKindMap: Record<SymbolKind, vscode.SymbolKind> = {
    'function': vscode.SymbolKind.Function,
    'variable': vscode.SymbolKind.Variable,
    'local': vscode.SymbolKind.Variable,
    'loop-variable': vscode.SymbolKind.Variable,
};

const tokenTypeIndex = new Map(
    (Object.keys(witToVscode) as TokenType[]).map((wit, i) => [wit, i])
);

// ============================================================================
// Extension State (minimal - just VS Code resources)
// ============================================================================

let ide: Root | null = null;
let diagnosticCollection: vscode.DiagnosticCollection;
let outputChannel: vscode.OutputChannel;
let projectTreeProvider: ProjectTreeProvider | null = null;

// ============================================================================
// WASM Loading
// ============================================================================

async function loadWasm(context: vscode.ExtensionContext): Promise<Root> {
    if (ide) return ide;

    const { instantiate } = require('./rpl-ide/rpl_ide.js');

    const cli = require('@bytecodealliance/preview2-shim/cli');
    const filesystem = require('@bytecodealliance/preview2-shim/filesystem');
    const io = require('@bytecodealliance/preview2-shim/io');
    const random = require('@bytecodealliance/preview2-shim/random');

    const wasmDir = path.join(context.extensionPath, 'src', 'rpl-ide');

    const getCoreModule = (wasmPath: string) => {
        const fullPath = path.join(wasmDir, wasmPath);
        const bytes = fs.readFileSync(fullPath);
        return new WebAssembly.Module(bytes);
    };

    const imports = {
        'wasi:cli/environment': cli.environment,
        'wasi:cli/exit': cli.exit,
        'wasi:cli/stderr': cli.stderr,
        'wasi:cli/stdin': cli.stdin,
        'wasi:cli/stdout': cli.stdout,
        'wasi:cli/terminal-input': cli.terminalInput,
        'wasi:cli/terminal-output': cli.terminalOutput,
        'wasi:cli/terminal-stderr': cli.terminalStderr,
        'wasi:cli/terminal-stdin': cli.terminalStdin,
        'wasi:cli/terminal-stdout': cli.terminalStdout,
        'wasi:filesystem/preopens': filesystem.preopens,
        'wasi:filesystem/types': filesystem.types,
        'wasi:io/error': io.error,
        'wasi:io/streams': io.streams,
        'wasi:random/insecure-seed': random.insecureSeed,
    };

    ide = await instantiate(getCoreModule, imports);
    return ide!;
}

// ============================================================================
// Extension Activation
// ============================================================================

export async function activate(context: vscode.ExtensionContext) {
    diagnosticCollection = vscode.languages.createDiagnosticCollection('rpl');
    outputChannel = vscode.window.createOutputChannel('RPL');

    try {
        await loadWasm(context);
    } catch (e: any) {
        vscode.window.showErrorMessage(`Failed to load RPL WASM: ${e.message}`);
        return;
    }

    // Document events
    context.subscriptions.push(
        vscode.workspace.onDidOpenTextDocument(checkDocument),
        vscode.workspace.onDidSaveTextDocument(checkDocument),
        vscode.workspace.onDidChangeTextDocument(e => checkDocument(e.document))
    );

    // Commands
    context.subscriptions.push(
        vscode.commands.registerCommand('rpl.runFile', runFile),
        vscode.commands.registerCommand('rpl.runProject', runProject),
        vscode.commands.registerCommand('rpl.openRepl', () => openRepl(context)),
        vscode.commands.registerCommand('rpl.disassemble', showDisassembly)
    );

    // Project tree view
    projectTreeProvider = new ProjectTreeProvider();
    vscode.window.registerTreeDataProvider('rplProjectOutline', projectTreeProvider);

    // Language providers
    context.subscriptions.push(
        vscode.languages.registerDocumentSemanticTokensProvider(
            { language: 'rpl' },
            new RplSemanticTokensProvider(),
            legend
        ),
        vscode.languages.registerHoverProvider(
            { language: 'rpl' },
            new RplHoverProvider()
        ),
        vscode.languages.registerDocumentSymbolProvider(
            { language: 'rpl' },
            new RplDocumentSymbolProvider()
        )
    );

    // Check open documents (this also auto-loads any projects)
    vscode.workspace.textDocuments.forEach(checkDocument);
    updateProjectContext();
}

// ============================================================================
// Document Checking
// ============================================================================

function checkDocument(document: vscode.TextDocument) {
    if (!ide || document.languageId !== 'rpl') return;

    // WASM auto-detects and loads projects from file path
    const diagnostics = ide.file.check(document.getText(), document.uri.fsPath);

    const vsDiags = diagnostics.map(d => new vscode.Diagnostic(
        new vscode.Range(
            d.range.start.line - 1, d.range.start.character - 1,
            d.range.end.line - 1, d.range.end.character - 1
        ),
        d.message,
        severityMap[d.severity]
    ));
    diagnosticCollection.set(document.uri, vsDiags);

    // Update UI if projects changed
    updateProjectContext();
}

function updateProjectContext() {
    if (!ide) return;
    const hasProjects = ide.project.listOpen().length > 0;
    vscode.commands.executeCommand('setContext', 'rpl.hasProject', hasProjects);
    projectTreeProvider?.refresh();
}

// ============================================================================
// Run Commands
// ============================================================================

function runFile() {
    if (!ide) return;

    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.languageId !== 'rpl') {
        vscode.window.showErrorMessage('No RPL file open');
        return;
    }

    outputChannel.clear();
    outputChannel.show(true);

    // WASM auto-detects project and runs entry point if applicable
    const result = ide.file.run(editor.document.getText(), editor.document.uri.fsPath);
    displayResult(result);
}

function runProject() {
    if (!ide) return;

    const editor = vscode.window.activeTextEditor;
    if (!editor) {
        vscode.window.showErrorMessage('No file open');
        return;
    }

    // Ask WASM for the project path
    const projectPath = ide.file.projectPath(editor.document.uri.fsPath);
    if (!projectPath) {
        vscode.window.showErrorMessage('Current file is not in a project');
        return;
    }

    outputChannel.clear();
    outputChannel.show(true);

    const result = ide.project.run(projectPath);
    displayResult(result);
}

function displayResult(result: { stack: Array<{ display: string; typeName: string }>; error?: string }) {
    if (result.error) {
        outputChannel.appendLine(`Error: ${result.error}`);
    } else {
        outputChannel.appendLine('Stack:');
        result.stack.forEach((v, i) => {
            outputChannel.appendLine(`  ${i}: ${v.display} (${v.typeName})`);
        });
    }
}

function showDisassembly() {
    if (!ide) return;

    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.languageId !== 'rpl') {
        vscode.window.showErrorMessage('No RPL file open');
        return;
    }

    // Call WASM disassemble function - returns formatted string
    const output = (ide.file as any).disassemble(
        editor.document.getText(),
        editor.document.uri.fsPath
    );

    outputChannel.clear();
    outputChannel.appendLine('=== Disassembly ===\n');
    outputChannel.appendLine(output);
    outputChannel.show(true);
}

// ============================================================================
// Language Providers
// ============================================================================

class RplSemanticTokensProvider implements vscode.DocumentSemanticTokensProvider {
    provideDocumentSemanticTokens(document: vscode.TextDocument): vscode.SemanticTokens | null {
        if (!ide) return null;

        const tokens = ide.file.tokens(document.getText());
        const builder = new vscode.SemanticTokensBuilder(legend);

        for (const t of tokens) {
            const typeIdx = tokenTypeIndex.get(t.tokenType) ?? tokenTypeIndex.get('variable')!;
            builder.push(t.line - 1, t.start - 1, t.length, typeIdx, 0);
        }

        return builder.build();
    }
}

class RplHoverProvider implements vscode.HoverProvider {
    provideHover(document: vscode.TextDocument, position: vscode.Position): vscode.Hover | null {
        if (!ide) return null;

        const hover = ide.file.hover(
            document.getText(),
            { line: position.line + 1, character: position.character + 1 },
            document.uri.fsPath
        );

        if (!hover) return null;

        return new vscode.Hover(
            new vscode.MarkdownString(hover.contents),
            new vscode.Range(
                hover.range.start.line - 1, hover.range.start.character - 1,
                hover.range.end.line - 1, hover.range.end.character - 1
            )
        );
    }
}

class RplDocumentSymbolProvider implements vscode.DocumentSymbolProvider {
    provideDocumentSymbols(document: vscode.TextDocument): vscode.DocumentSymbol[] {
        if (!ide) return [];

        return ide.file.symbols(document.getText()).map(sym => new vscode.DocumentSymbol(
            sym.name,
            sym.detail ?? '',
            symbolKindMap[sym.kind],
            new vscode.Range(
                sym.range.start.line - 1, sym.range.start.character - 1,
                sym.range.end.line - 1, sym.range.end.character - 1
            ),
            new vscode.Range(
                sym.selectionRange.start.line - 1, sym.selectionRange.start.character - 1,
                sym.selectionRange.end.line - 1, sym.selectionRange.end.character - 1
            )
        ));
    }
}

// ============================================================================
// Project Tree View
// ============================================================================

class ProjectTreeItem extends vscode.TreeItem {
    constructor(
        public readonly projectPath: string,
        public readonly key: string,
        public readonly isProjectRoot: boolean,
        public readonly isDirectory: boolean,
        label: string,
        description?: string
    ) {
        super(
            label,
            (isProjectRoot || isDirectory)
                ? vscode.TreeItemCollapsibleState.Expanded
                : vscode.TreeItemCollapsibleState.None
        );
        this.description = description;
        this.tooltip = isProjectRoot ? projectPath : key;
        this.iconPath = isProjectRoot
            ? new vscode.ThemeIcon('folder-library')
            : isDirectory
                ? undefined
                : new vscode.ThemeIcon('symbol-function');
    }
}

class ProjectTreeProvider implements vscode.TreeDataProvider<ProjectTreeItem> {
    private _onDidChangeTreeData = new vscode.EventEmitter<ProjectTreeItem | undefined>();
    readonly onDidChangeTreeData = this._onDidChangeTreeData.event;

    refresh() {
        this._onDidChangeTreeData.fire(undefined);
    }

    getTreeItem(element: ProjectTreeItem) {
        return element;
    }

    getChildren(element?: ProjectTreeItem): ProjectTreeItem[] {
        if (!ide) return [];

        // Root level: show all open projects (from WASM)
        if (!element) {
            return ide.project.listOpen().sort().map(projectPath => {
                const name = path.basename(projectPath);
                return new ProjectTreeItem(projectPath, '', true, false, name, projectPath);
            });
        }

        // Get tree for this project
        const nodes = ide.project.tree(element.projectPath);
        const prefix = element.isProjectRoot ? '' : element.key + '/';

        // Build immediate children at this level
        const dirs = new Set<string>();
        const entries: ProjectTreeItem[] = [];

        for (const n of nodes) {
            if (prefix && !n.key.startsWith(prefix)) continue;

            const rest = prefix ? n.key.slice(prefix.length) : n.key;
            const slashIdx = rest.indexOf('/');

            if (slashIdx === -1) {
                entries.push(new ProjectTreeItem(
                    element.projectPath, n.key, false, false, n.name, n.signature ?? undefined
                ));
            } else {
                dirs.add(rest.slice(0, slashIdx));
            }
        }

        const dirItems = Array.from(dirs).sort().map(dir =>
            new ProjectTreeItem(element.projectPath, prefix + dir, false, true, dir)
        );

        entries.sort((a, b) => a.label!.toString().localeCompare(b.label!.toString()));
        return [...dirItems, ...entries];
    }
}

// ============================================================================
// REPL
// ============================================================================

let replPanel: vscode.WebviewPanel | null = null;

function openRepl(context: vscode.ExtensionContext) {
    if (!ide) return;

    if (replPanel) {
        replPanel.reveal();
        return;
    }

    replPanel = vscode.window.createWebviewPanel(
        'rplRepl', 'RPL REPL', vscode.ViewColumn.Two, { enableScripts: true }
    );

    replPanel.webview.html = getReplHtml();

    replPanel.webview.onDidReceiveMessage(msg => {
        if (!ide || !replPanel) return;
        if (msg.command === 'eval') {
            replPanel.webview.postMessage({ command: 'result', result: ide.repl.evaluate(msg.code) });
        } else if (msg.command === 'reset') {
            ide.repl.reset();
            replPanel.webview.postMessage({ command: 'reset' });
        }
    }, undefined, context.subscriptions);

    replPanel.onDidDispose(() => { replPanel = null; }, null, context.subscriptions);
}

function getReplHtml() {
    return `<!DOCTYPE html>
<html>
<head>
    <style>
        body { font-family: var(--vscode-font-family); padding: 10px; }
        #output {
            height: 300px; overflow-y: auto;
            border: 1px solid var(--vscode-input-border);
            padding: 10px; margin-bottom: 10px; font-family: monospace;
        }
        .error { color: var(--vscode-errorForeground); }
        .stack { color: var(--vscode-foreground); }
        #input {
            width: 100%; padding: 8px;
            border: 1px solid var(--vscode-input-border);
            background: var(--vscode-input-background);
            color: var(--vscode-input-foreground);
            font-family: monospace;
        }
        button { margin-top: 10px; padding: 5px 15px; cursor: pointer; }
    </style>
</head>
<body>
    <div id="output"></div>
    <input type="text" id="input" placeholder="Enter RPL code..." />
    <button onclick="resetRepl()">Reset</button>
    <script>
        const vscode = acquireVsCodeApi();
        const output = document.getElementById('output');
        const input = document.getElementById('input');
        input.addEventListener('keypress', e => {
            if (e.key === 'Enter' && input.value.trim()) {
                output.innerHTML += '<div>> ' + input.value + '</div>';
                vscode.postMessage({ command: 'eval', code: input.value });
                input.value = '';
            }
        });
        function resetRepl() { vscode.postMessage({ command: 'reset' }); }
        window.addEventListener('message', e => {
            const msg = e.data;
            if (msg.command === 'result') {
                if (msg.result.error) {
                    output.innerHTML += '<div class="error">Error: ' + msg.result.error + '</div>';
                } else {
                    const stack = msg.result.stack.map((v, i) => i + ': ' + v.display + ' (' + v.typeName + ')').join('\\n');
                    output.innerHTML += '<div class="stack">' + stack + '</div>';
                }
                output.scrollTop = output.scrollHeight;
            } else if (msg.command === 'reset') {
                output.innerHTML += '<div>--- Stack cleared ---</div>';
            }
        });
    </script>
</body>
</html>`;
}

// ============================================================================
// Deactivation
// ============================================================================

export function deactivate() {
    if (ide) {
        for (const projectPath of ide.project.listOpen()) {
            ide.project.close(projectPath);
        }
    }
}
