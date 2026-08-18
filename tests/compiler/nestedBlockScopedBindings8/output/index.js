// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/nestedBlockScopedBindings8.ts`, Apache-2.0 License
var x;
for ( var x; false; ) {
  () => (x);
}
var y;
for ( var y; false; ) {
  y = 1;
}