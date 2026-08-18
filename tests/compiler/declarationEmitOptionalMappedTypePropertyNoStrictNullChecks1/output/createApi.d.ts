type Id<T> = {
[K in keyof T]: T[K]
} & {};
export function createApi<Definitions>(_: {
  endpoints: Definitions;
}): {
[K in keyof Definitions`use${Capitalize<K & string>Query}`]: () => Id<{
  status: "uninitialized";
  originalArgs?: undefined;
}>
};
