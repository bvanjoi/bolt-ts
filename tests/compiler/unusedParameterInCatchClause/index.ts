// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedParameterInCatchClause.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noEmit
//@compiler-options: noUnusedLocals

function f1() {
    try {} catch(ex){}
}