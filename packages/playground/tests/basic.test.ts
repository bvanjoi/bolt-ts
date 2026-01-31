import { add } from '@bolt-ts/napi'
import { expect, test } from '@playwright/experimental-ct-react'

test('basic test', () => {
	expect(add(1, 2)).toBe(3)
})
