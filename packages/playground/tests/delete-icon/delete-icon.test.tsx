import { expect, test } from '@playwright/experimental-ct-react'
import { DeleteIcon_ID_IS_SAME_WHEN_CLICK } from './fixture'

test('`DeleteIcon` - click is means handle delete', async ({ mount, page }) => {
	await mount(<DeleteIcon_ID_IS_SAME_WHEN_CLICK />)
	expect(await page.locator('#count').textContent()).toBe('0')
	await page.locator('button').click()
	expect(await page.locator('#count').textContent()).toBe('1')
})

test.only('`DeleteIcon` - tailwind classname', async ({ mount, page }) => {
	await mount(<DeleteIcon_ID_IS_SAME_WHEN_CLICK />)
	const button = page.locator('button')
	await button.hover()
	const cn: string = await button.evaluate(el => el.className)
	expect(cn).toContain('hover:cursor-pointer')
})
