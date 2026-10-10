// From `github.com/microsoft/TypeScript/blob/v5.0.3/tests/cases/compiler/breakTarget5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnusedLabels

target:
while (true) {
  function f() {
    while (true) {
      break target;
      //~^ ERROR: Jump target cannot cross function boundary.
    }
  }
}