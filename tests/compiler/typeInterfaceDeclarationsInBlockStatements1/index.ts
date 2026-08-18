// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeInterfaceDeclarationsInBlockStatements1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration

// https://github.com/microsoft/TypeScript/issues/60175

function f1() {
  if (true) type s = string;
  //~^ ERROR: 'type' declarations can only be declared inside a block.
  console.log("" as s);
}

function f2() {
  if (true) {
    type s = string;
  }
  console.log("" as s);
  //~^ ERROR: Cannot find name 's'.
}

function f3() {
  if (true)
    interface s {
      //~^ ERROR: 'interface' declarations can only be declared inside a block.
      length: number;
    }
  console.log("" as s);
}

function f4() {
  if (true) {
    interface s {
      length: number;
    }
  }
  console.log("" as s);
  //~^ ERROR: Cannot find name 's'.
}
