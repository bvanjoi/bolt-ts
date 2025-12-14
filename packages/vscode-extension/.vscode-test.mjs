import { defineConfig } from '@vscode/test-cli'

export default defineConfig({
	files: 'out/tests/**/*.test.js',
	launchArgs: ['--disable-extensions']
})
