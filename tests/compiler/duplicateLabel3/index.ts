// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/duplicateLabel3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: allowUnusedLabels

target:
while (true) {
  function f() {
    target:
    while (true) {
    }
  }
}

let a = number;
//~^ ERROR: Cannot find name 'number'.