// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedMethodsInInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

interface I1 {
    f1();
    f2(x: number, y: string);
}