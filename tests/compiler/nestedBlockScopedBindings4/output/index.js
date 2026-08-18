// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function a0() {
  for ( var x; x < 1; ) {
    x = x + 1;
  }
  for ( var x; ; ) {
    x = x + 2;
  }
}
function a1() {
  for ( var x; x < 1; ) {
    x = x + 1;
    () => (x);
  }
  for ( var x; ; ) {
    x = x + 2;
  }
}
function a2() {
  for ( var x; x < 1; ) {
    x = x + 1;
  }
  for ( var x; ; ) {
    x = x + 2;
    () => (x);
  }
}
function a3() {
  for ( var x; x < 1; ) {
    x = x + 1;
    () => (x);
  }
  for ( var x; ; ) {
    x = x + 2;
    () => (x);
  }
}