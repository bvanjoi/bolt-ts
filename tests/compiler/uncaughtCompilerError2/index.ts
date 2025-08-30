// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/uncaughtCompilerError2.ts`, Apache-2.0 License

function getObj() {
   ().toString();
   //~^ ERROR: Expression expected.
}
