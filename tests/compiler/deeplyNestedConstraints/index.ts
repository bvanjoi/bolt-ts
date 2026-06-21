// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/deeplyNestedConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration

type Enum = Record<string, string | number>;

type TypeMap<E extends Enum> = { [key in E[keyof E]]: number | boolean | string | number[] };

class BufferPool<E extends Enum, M extends TypeMap<E>> {
    setArray2<K extends E[keyof E]>(_: K, array: Extract<M[K], ArrayLike<any>>) {
        array.length; // Requires exploration of >5 levels of constraints
    }
}