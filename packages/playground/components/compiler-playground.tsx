"use client"

import { useState, useCallback } from "react"
import { FileTabs } from "./file-tabs"
import { CodeEditor } from "./code-editor"
import { OutputPanel } from "./output-panel"
import { Play } from "lucide-react"

export interface SourceFile {
  id: string
  name: string
  content: string
  language: string
}

export interface CompileResult {
  output: string
  ast: string
  errors: string[]
}

const defaultFiles: SourceFile[] = [
  {
    id: "1",
    name: "main.js",
    content: `function greet(name) {
  return "Hello, " + name + "!";
}

const message = greet("World");
console.log(message);`,
    language: "javascript",
  },
]

export function CompilerPlayground() {
  const [files, setFiles] = useState<SourceFile[]>(defaultFiles)
  const [activeFileId, setActiveFileId] = useState(defaultFiles[0].id)
  const [result, setResult] = useState<CompileResult>({
    output: "",
    ast: "",
    errors: [],
  })
  const [isCompiling, setIsCompiling] = useState(false)

  const activeFile = files.find((f) => f.id === activeFileId) || files[0]

  const handleCodeChange = useCallback(
    (value: string | undefined) => {
      setFiles((prev) => prev.map((f) => (f.id === activeFileId ? { ...f, content: value || "" } : f)))
    },
    [activeFileId],
  )

  const handleAddFile = useCallback(() => {
    const newId = Date.now().toString()
    const fileNum = files.length + 1
    const newFile: SourceFile = {
      id: newId,
      name: `file${fileNum}.js`,
      content: "// New file\n",
      language: "javascript",
    }
    setFiles((prev) => [...prev, newFile])
    setActiveFileId(newId)
  }, [files.length])

  const handleRemoveFile = useCallback(
    (id: string) => {
      if (files.length <= 1) return
      setFiles((prev) => prev.filter((f) => f.id !== id))
      if (activeFileId === id) {
        setActiveFileId(files[0].id === id ? files[1]?.id : files[0].id)
      }
    },
    [files, activeFileId],
  )

  const handleRenameFile = useCallback((id: string, newName: string) => {
    setFiles((prev) => prev.map((f) => (f.id === id ? { ...f, name: newName } : f)))
  }, [])

  const handleCompile = useCallback(async () => {
    setIsCompiling(true)

    // Simulate compilation with mock AST generation
    await new Promise((r) => setTimeout(r, 300))

    const allCode = files.map((f) => `// === ${f.name} ===\n${f.content}`).join("\n\n")

    try {
      // Mock AST generation
      const mockAst = {
        type: "Program",
        body: files.map((f) => ({
          type: "File",
          name: f.name,
          statements: f.content.split("\n").filter(Boolean).length,
        })),
      }

      // Try to evaluate for demo purposes
      let output = ""
      const logs: string[] = []
      const mockConsole = {
        log: (...args: unknown[]) => logs.push(args.map(String).join(" ")),
      }

      try {
        const fn = new Function("console", files[0].content)
        fn(mockConsole)
        output = logs.join("\n") || "(no output)"
      } catch (e) {
        output = "(evaluation skipped)"
      }

      setResult({
        output,
        ast: JSON.stringify(mockAst, null, 2),
        errors: [],
      })
    } catch (error) {
      setResult({
        output: "",
        ast: "",
        errors: [error instanceof Error ? error.message : "Unknown error"],
      })
    }

    setIsCompiling(false)
  }, [files])

  return (
    <div className="h-screen flex flex-col bg-[#fafafa]">
      {/* Header */}
      <header className="flex items-center justify-between px-4 py-3 border-b border-[#e5e5e5] bg-white">
        <div className="flex items-center gap-3">
          <div className="flex items-center gap-2">
            <span className="text-xl font-bold text-[#222]">⚡</span>
            <span className="text-xl font-bold text-[#222]">Compiler</span>
          </div>
          <span className="text-sm text-[#666]">Playground</span>
        </div>
        <button
          onClick={handleCompile}
          disabled={isCompiling}
          className="flex items-center gap-2 px-4 py-1.5 bg-[#222] text-white text-sm font-medium rounded hover:bg-[#333] disabled:opacity-50 transition-colors"
        >
          <Play size={14} />
          {isCompiling ? "Compiling..." : "Compile"}
        </button>
      </header>

      {/* Main Content */}
      <div className="flex-1 flex min-h-0">
        {/* Left Panel - Editor */}
        <div className="flex-1 flex flex-col border-r border-[#e5e5e5]">
          <div className="flex items-center justify-between px-2 py-1 bg-[#f5f5f5] border-b border-[#e5e5e5]">
            <FileTabs
              files={files}
              activeFileId={activeFileId}
              onSelect={setActiveFileId}
              onAdd={handleAddFile}
              onRemove={handleRemoveFile}
              onRename={handleRenameFile}
            />
            <span className="text-xs text-[#999] uppercase tracking-wide pr-2">Input</span>
          </div>
          <div className="flex-1">
            <CodeEditor value={activeFile.content} language={activeFile.language} onChange={handleCodeChange} />
          </div>
        </div>

        {/* Divider */}
        <div className="w-[3px] bg-[#f5c518]" />

        {/* Right Panel - Output */}
        <div className="flex-1 flex flex-col">
          <OutputPanel result={result} />
        </div>
      </div>
    </div>
  )
}
