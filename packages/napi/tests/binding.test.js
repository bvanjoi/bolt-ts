const { deepStrictEqual } = require('node:assert/strict')
const { test } = require('node:test')
const { add: addNode } = require('@bolt-ts/napi')
const { add: addWasm } = require('@bolt-ts/napi/wasm-node')

test('test node binding', () => {
	deepStrictEqual(addNode(1, 2), 3)
})

test('test wasm binding', () => {
	deepStrictEqual(addWasm(1, 2), 3)
})
