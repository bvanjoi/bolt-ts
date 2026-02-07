// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/exportSameNameFuncVar.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015

export var a = 10;
export function a() { //~ERROR: Duplicate identifier 'a'.
}