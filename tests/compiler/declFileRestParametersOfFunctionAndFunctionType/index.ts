// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileRestParametersOfFunctionAndFunctionType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

function f1(...args) { }
function f2(x: (...args) => void) { }
function f3(x: { (...args): void }) { }
function f4<T extends (...args) => void>() { }
function f5<T extends { (...args): void }>() { }
var f6 = () => { return [<any>10]; }


