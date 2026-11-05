// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/breakNotInIterationOrSwitchStatement2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

while (true) {
  function f() {
    break;
    //~^ ERROR: A 'break' statement can only be used within an enclosing iteration or switch statement.
  }
}