'use client'

import Editor from '@monaco-editor/react'

interface CodeEditorProps {
	value: string
	language: string
	onChange: (value: string | undefined) => void
}

export function CodeEditor({ value, language, onChange }: CodeEditorProps) {
	return (
		<Editor
			height="100%"
			language={language}
			value={value}
			onChange={onChange}
			theme="vs"
			options={{
				minimap: { enabled: false },
				fontSize: 14,
				fontFamily: "'Geist Mono', monospace",
				lineNumbers: 'on',
				scrollBeyondLastLine: false,
				automaticLayout: true,
				padding: { top: 12, bottom: 12 },
				renderLineHighlight: 'none',
				overviewRulerBorder: false,
				hideCursorInOverviewRuler: true,
				scrollbar: {
					vertical: 'auto',
					horizontal: 'auto',
					verticalScrollbarSize: 8,
					horizontalScrollbarSize: 8
				}
			}}
		/>
	)
}
