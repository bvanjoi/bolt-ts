// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/undefinedAssignableToGenericMappedIntersection.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type Errors<T> = { [P in keyof T]: string | undefined } & {all: string | undefined};
function foo<T>() {
    let obj!: Errors<T>
    let x!: keyof T;
    obj[x] = undefined;
}
