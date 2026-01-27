import { X } from 'lucide-react'
import type { MouseEvent } from 'react'

interface DeleteIconProps<T> {
	deleteId: T
	handleDelete: (event: MouseEvent, deleteId: T) => void
}

export function DeleteIcon<T>(props: DeleteIconProps<T>) {
	return (
		<button
			type="button"
			onClick={e => {
				e.stopPropagation()
				props.handleDelete(e, props.deleteId)
			}}
			className="opacity-0 group-hover:opacity-100 p-0.5 hover:bg-[#ddd] rounded transition-opacity"
		>
			<X size={12} />
		</button>
	)
}
