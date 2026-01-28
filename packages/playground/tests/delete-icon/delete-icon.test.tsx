import { expect, test } from '@playwright/experimental-ct-react'
import { DeleteIcon_ID_IS_SAME_WHEN_CLICK } from './fixture'

test('`DeleteIcon` - click is means handle delete', async ({ mount, page }) => {
	await mount(<DeleteIcon_ID_IS_SAME_WHEN_CLICK />)
	expect(await page.locator('#count').textContent()).toBe('0')
	await page.locator('button').click()
	expect(await page.locator('#count').textContent()).toBe('1')
})
