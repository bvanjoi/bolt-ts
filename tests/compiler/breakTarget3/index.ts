// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/breakTarget3.ts`, Apache-2.0 License

//@compiler-options: allowUnusedLabels

target1:
target2:
while (true) {
  break target1;
}