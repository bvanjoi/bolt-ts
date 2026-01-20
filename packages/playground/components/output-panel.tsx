"use client"

import { useState } from "react"
import type { CompileResult } from "./compiler-playground"

interface OutputPanelProps {
  result: CompileResult
}

type TabType = "output" | "ast" | "errors"

export function OutputPanel({ result }: OutputPanelProps) {
  const [activeTab, setActiveTab] = useState<TabType>("output")

  const tabs: { id: TabType; label: string; badge?: number }[] = [
    { id: "output", label: "Output" },
    { id: "ast", label: "AST" },
    { id: "errors", label: "Errors", badge: result.errors.length || undefined },
  ]

  const renderContent = () => {
    switch (activeTab) {
      case "output":
        return (
          <pre className="p-4 text-sm font-mono text-[#222] whitespace-pre-wrap">{result.output || "(no output)"}</pre>
        )
      case "ast":
        return (
          <pre className="p-4 text-sm font-mono text-[#222] whitespace-pre-wrap overflow-auto">
            {result.ast || "(compile to see AST)"}
          </pre>
        )
      case "errors":
        return result.errors.length > 0 ? (
          <div className="p-4 space-y-2">
            {result.errors.map((error, i) => (
              <div key={i} className="p-3 bg-red-50 border border-red-200 rounded text-sm text-red-700 font-mono">
                {error}
              </div>
            ))}
          </div>
        ) : (
          <div className="p-4 text-sm text-[#999]">No errors</div>
        )
    }
  }

  return (
    <div className="h-full flex flex-col">
      <div className="flex items-center justify-between px-2 py-1 bg-[#f5f5f5] border-b border-[#e5e5e5]">
        <div className="flex items-center gap-1">
          {tabs.map((tab) => (
            <button
              key={tab.id}
              onClick={() => setActiveTab(tab.id)}
              className={`flex items-center gap-1.5 px-3 py-1 text-sm rounded transition-colors ${
                activeTab === tab.id
                  ? "bg-white text-[#222] shadow-sm"
                  : "text-[#666] hover:text-[#222] hover:bg-[#eee]"
              }`}
            >
              {tab.label}
              {tab.badge && (
                <span className="px-1.5 py-0.5 text-xs bg-red-500 text-white rounded-full">{tab.badge}</span>
              )}
            </button>
          ))}
        </div>
        <span className="text-xs text-[#999] uppercase tracking-wide pr-2">Output</span>
      </div>
      <div className="flex-1 overflow-auto bg-white">{renderContent()}</div>
    </div>
  )
}
