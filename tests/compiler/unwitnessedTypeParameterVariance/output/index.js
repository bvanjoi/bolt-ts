// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unwitnessedTypeParameterVariance.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
function foo() {
  var unk = {
      read: (origin) => (unk)    
  };
  var x = unk;
}


b = a;