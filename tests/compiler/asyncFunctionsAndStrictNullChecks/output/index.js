// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/asyncFunctionsAndStrictNullChecks.ts`, Apache-2.0 License
//@compiler-options: target=es6

async function sample(promise) {
  var number = await promise;
}
async function sample2(x) {
  var x1 = await resolve1(x);
  var x2 = await resolve2(x);
}