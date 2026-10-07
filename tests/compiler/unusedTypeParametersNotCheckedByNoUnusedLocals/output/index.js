// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedTypeParametersNotCheckedByNoUnusedLocals.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
function f() {}
;
class C {
  m() {}
}
;
var l = () => {};