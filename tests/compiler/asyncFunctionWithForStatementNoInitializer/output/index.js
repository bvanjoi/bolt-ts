// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/asyncFunctionWithForStatementNoInitializer.ts`, Apache-2.0 License
async function test1() {
  var i = 0;
  var limit = 10;
  for ( ; i < limit; ++i) {}
}
async function test2() {
  var i = 0;
  var limit = 10;
  for ( i = 1; i < limit; ++i) {}
}
async function test3() {
  var i = 0;
  for ( ; ; ++i) {}
}
async function test4() {
  for ( ; ; ) {}
}