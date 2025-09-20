// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/breakNotInIterationOrSwitchStatement2.ts`, Apache-2.0 License

while (true) {
  function f() {
    break;
    //~^ ERROR: A 'break' statement can only be used within an enclosing iteration or switch statement.
  }
}