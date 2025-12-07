import * as vscode from 'vscode'

export function activate(context: vscode.ExtensionContext) {
	console.log('Congratulations, your extension "bolt-ts" is now active!')

	const disposable = vscode.commands.registerCommand(
		'bolt-ts.helloWorld',
		() => {
			vscode.window.showInformationMessage('Hello World from bolt-ts!')
		},
	)

	context.subscriptions.push(disposable)
}

export function deactivate() {}
