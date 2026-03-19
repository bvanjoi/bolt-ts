// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrayConcat3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictFunctionTypes

type Fn<T extends object> = <U extends T>(subj: U) => U
function doStuff<T extends object, T1 extends T>(a: Array<Fn<T>>, b: Array<Fn<T1>>) {
    b.concat(a);
}
