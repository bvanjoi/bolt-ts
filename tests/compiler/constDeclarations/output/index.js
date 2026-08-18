// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constDeclarations.ts`, Apache-2.0 License
//@compiler-options: target=es6
//@compiler-options: strict=false
//@compiler-options: declaration
var c1 = false;
var c2 = 23;
var c3 = 0, c4 = '', c5 = null;
for ( var c4 = 0; c4 < 9; ) {
  break;
}
for ( var c5 = 0, c6 = 0; c5 < c6; ) {
  break;
}
