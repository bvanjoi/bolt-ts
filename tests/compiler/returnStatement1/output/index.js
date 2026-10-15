// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/returnStatement1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: allowUnreachableCode
function f() {
  return function (s) {
    var x = s;
  };
  ('harmless extra line');
}