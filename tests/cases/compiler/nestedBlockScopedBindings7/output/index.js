// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nestedBlockScopedBindings7.ts`, Apache-2.0 License
for ( var x; false; ) {
  () => x;
}
for ( var y; false; ) {
  y = 1;
}