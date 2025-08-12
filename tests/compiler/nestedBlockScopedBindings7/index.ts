// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/nestedBlockScopedBindings7.ts`, Apache-2.0 License

for (let x; false;) {
  () => x;
}

for (let y; false;) {
  y = 1;
}