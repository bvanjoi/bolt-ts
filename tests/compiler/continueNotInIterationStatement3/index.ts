// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/continueNotInIterationStatement3.ts`, Apache-2.0 License

switch (0) {
  default:
    continue;
    //~^ ERROR: A 'continue' statement can only be used within an enclosing iteration statement.
}