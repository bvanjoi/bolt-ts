import React from 'react'
import { DeleteIcon } from '../../components/delete-icon'

export function DeleteIcon_ID_IS_SAME_WHEN_CLICK() {
	const [count, setCount] = React.useState(0)

	return (
		<>
			<DeleteIcon
				deleteId="id"
				handleDelete={(_, id) => {
					if (id !== 'id') {
						throw new Error('Unexpected id')
					}
					setCount(count + 1)
				}}
			/>
			<div id="count">{count}</div>
		</>
	)
}
