// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitReturnsInAsync1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
async function test(isError = false) {
  if (isError === true) {
    return ;
  }
  
  var x = await Promise.resolve('The test is passed without an error.');
}