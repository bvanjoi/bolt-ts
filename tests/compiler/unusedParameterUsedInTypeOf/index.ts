// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedParameterUsedInTypeOf.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function f1 (a: number, b: typeof a) {
    return b;
}
