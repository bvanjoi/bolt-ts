// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/continueTarget5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnreachableCode

target:
while (true) {
  function f() {
    while (true) {
      continue target;
      //~^ ERROR: Jump target cannot cross function boundary.
    }
  }
}