'use client'

import { Plus, X } from 'lucide-react'
import type React from 'react'
import { useState } from 'react'
import type { SourceFile } from './compiler-playground'

interface FileTabsProps {
	files: SourceFile[]
	activeFileId: string
	onSelect: (id: string) => void
	onAdd: () => void
	onRemove: (id: string) => void
	onRename: (id: string, name: string) => void
}

export function FileTabs({
	files,
	activeFileId,
	onSelect,
	onAdd,
	onRemove,
	onRename
}: FileTabsProps) {
	const [editingId, setEditingId] = useState<string | null>(null)
	const [editName, setEditName] = useState('')

	const handleDoubleClick = (file: SourceFile) => {
		setEditingId(file.id)
		setEditName(file.name)
	}

	const handleBlur = (id: string) => {
		if (editName.trim()) {
			onRename(id, editName.trim())
		}
		setEditingId(null)
	}

	const handleKeyDown = (e: React.KeyboardEvent, id: string) => {
		if (e.key === 'Enter') {
			handleBlur(id)
		} else if (e.key === 'Escape') {
			setEditingId(null)
		}
	}

	return (
		<div className="flex items-center gap-1">
			{files.map(file => (
				<div
					key={file.id}
					className={`group flex items-center gap-1 px-3 py-1 text-sm cursor-pointer rounded transition-colors ${
						activeFileId === file.id
							? 'bg-white text-[#222] shadow-sm'
							: 'text-[#666] hover:text-[#222] hover:bg-[#eee]'
					}`}
					onClick={() => onSelect(file.id)}
					onDoubleClick={() => handleDoubleClick(file)}
				>
					{editingId === file.id ? (
						<input
							type="text"
							value={editName}
							onChange={e => setEditName(e.target.value)}
							onBlur={() => handleBlur(file.id)}
							onKeyDown={e => handleKeyDown(e, file.id)}
							onClick={e => e.stopPropagation()}
							className="w-20 px-1 text-sm bg-white border border-[#ccc] rounded outline-none"
							autoFocus
						/>
					) : (
						<>
							<span className="font-mono text-xs">{file.name}</span>
							{files.length > 1 && (
								<button
									onClick={e => {
										e.stopPropagation()
										onRemove(file.id)
									}}
									className="opacity-0 group-hover:opacity-100 p-0.5 hover:bg-[#ddd] rounded transition-opacity"
								>
									<X size={12} />
								</button>
							)}
						</>
					)}
				</div>
			))}
			<button
				onClick={onAdd}
				className="p-1 text-[#666] hover:text-[#222] hover:bg-[#eee] rounded transition-colors"
				title="Add file"
			>
				<Plus size={16} />
			</button>
		</div>
	)
}
