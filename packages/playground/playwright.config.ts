import { defineConfig } from '@playwright/experimental-ct-react'

export default defineConfig({
	testMatch: ['tests/**/*.{test,e2e}.{tsx,ts}']
})
