// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nestedBlockScopedBindings13.ts`, Apache-2.0 License
for ( ; false; ) {
  var x;
  () => x;
}
for ( ; false; ) {
  var y;
  y = 1;
}