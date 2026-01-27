import { clsx } from 'clsx'
import { type ReactNode, useState } from 'react'

type FileTabInputProps = {
	filename: string
}

function FileTabInput(props: FileTabInputProps) {
	const [editName, setEditName] = useState(props.filename)
}

export function FilenameDisplay(props: { filename: string }) {
	return <span className="font-mono text-xs">{props.filename}</span>
}

type FileTabProps = {
	filename: string
	isActive: boolean
	isEditing: boolean

	handleClick?: (filename: string) => void
	handleDoubleClick?: (filename: string) => void

	DeleteIcon?: ReactNode
}

export function FileTab(props: FileTabProps) {
	const { filename: fileName, isActive, isEditing } = props
	return (
		<button
			type="button"
			key={fileName}
			className={clsx(
				'group flex items-center gap-1 px-3 py-1 text-sm cursor-pointer rounded transition-colors',
				isActive
					? 'bg-white text-[#222] shadow-sm'
					: 'text-[#666] hover:text-[#222] hover:bg-[#eee]'
			)}
			onClick={() => {
				if (props.handleClick) {
					props.handleClick(fileName)
				}
			}}
			onDoubleClick={() => {
				if (props.handleDoubleClick) {
					props.handleDoubleClick(fileName)
				}
			}}
		>
			{isEditing ? (
				// <FileTabInput filename={fileName} value="" />
				<></>
			) : (
				<>
					<FilenameDisplay filename={fileName} />
					{props.DeleteIcon}
				</>
			)}
		</button>
	)
}
