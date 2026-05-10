// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateLabel2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnusedLabels

target:
while (true) {
  target: //~ERROR: Duplicate label 'target'.
  while (true) {
  }
}