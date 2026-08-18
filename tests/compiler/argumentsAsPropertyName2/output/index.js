// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsAsPropertyName2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo() {
  for ( var x = 0; x < 1; ++x) {
    var i;
    [].forEach(function () {
      i;
    });
    ({
          arguments: 0      
    });
    ({
          arguments      
    });
    ({
          arguments: arguments      
    });
  }
}