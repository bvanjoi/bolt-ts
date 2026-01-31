import { ok } from 'node:assert/strict'

import { extensions } from 'vscode'

test('bolt-ts extension exist', async () => {
	const ext = extensions.getExtension('bohan.bolt-ts-vscode-extension')
	ok(ext)
})
