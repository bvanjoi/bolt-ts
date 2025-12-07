import * as assert from 'node:assert'

import * as vscode from 'vscode'

test('Commands of bolt-ts', async () => {
	const ret = await vscode.commands.executeCommand('bolt-ts.helloWorld')
	assert.strictEqual(ret, 42)
})
