// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/emptyMemberAccess.ts`, Apache-2.0 License

function getObj() {

   ().toString();
   //~^ ERROR: Expression expected.
}
 