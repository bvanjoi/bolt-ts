import * as assert from 'node:assert'

import * as vscode from 'vscode'

test('bolt-ts extension exist', async () => {
	const ext = vscode.extensions.getExtension('bohan.bolt-ts-vscode-extension')
	assert.ok(ext)
})
