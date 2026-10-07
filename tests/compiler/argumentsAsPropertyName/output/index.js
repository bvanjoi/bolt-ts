// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsAsPropertyName.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
function myFunction(myType) {
  for ( var i = 0; i < 10; i++) {
    use(myType.arguments[i]);
    // create closure so that tsc will turn loop body into function
    var x = 5;
    [1, 2, 3].forEach(function (j) {
      use(x);
    });
  }
}