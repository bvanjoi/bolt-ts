// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsInRecursiveReturn.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: noUnusedLocals

function recursive(arg: string, other: string) {
    const someLocalVar = arg + other;
    return recursive(someLocalVar, arg);
}