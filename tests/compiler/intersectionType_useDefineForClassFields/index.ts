// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionType_useDefineForClassFields.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: useDefineForClassFields

type Foo<T> = {
    [k in keyof T & string]: any
}

function bar<T>(_p: T): { new(): Foo<T> } {
    return null as any;
}

class Baz extends bar({ x: 1 }) {
}