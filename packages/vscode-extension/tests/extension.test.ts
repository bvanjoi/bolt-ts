import * as assert from 'node:assert'
import * as test from 'node:test'

import * as vscode from 'vscode'

// import * as myExtension from '../../extension';

test('Extension Test Suite', () => {
	vscode.window.showInformationMessage('Start all tests.')

	test('Sample test', () => {
		assert.strictEqual(-1, [1, 2, 3].indexOf(2))
		assert.strictEqual(-1, [1, 2, 3].indexOf(0))
	})
})
