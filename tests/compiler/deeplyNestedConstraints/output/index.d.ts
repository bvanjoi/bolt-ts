type Enum = Record<string, string | number>;
type TypeMap<E extends Enum> = {
[key in E[keyof E]]: number | boolean | string | number[]
};
declare class BufferPool<E extends Enum, M extends TypeMap<E>> {
  setArray2<K extends E[keyof E]>(_: K, array: Extract<M[K], ArrayLike<any>>);
}
