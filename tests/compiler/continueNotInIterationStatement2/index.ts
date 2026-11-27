// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/continueNotInIterationStatement2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

while (true) {
  function f() {
    continue;
    //~^ ERROR: A 'continue' statement can only be used within an enclosing iteration statement.
  }
}