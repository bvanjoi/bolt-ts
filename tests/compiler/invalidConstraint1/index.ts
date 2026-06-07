// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invalidConstraint1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function f<T, U extends { a: T }>() {
    return undefined;
}
f<string, { a: number }>(); // should error
//~^ ERROR: Type '{ a: number; }' does not satisfy the constraint '{ a: string; }'.
