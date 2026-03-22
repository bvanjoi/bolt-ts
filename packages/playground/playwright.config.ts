import path from 'node:path'
import { defineConfig } from '@playwright/experimental-ct-react'

export default defineConfig({
	testDir: path.resolve(__dirname, './tests'),
	testMatch: '*.test.{ts,tsx}'
})
