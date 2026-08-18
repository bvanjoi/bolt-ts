// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeGuardNarrowByUntypedField.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: lib=[es6]

if (hasOwnProperty(arrayLikeOrIterable, 'length')) {
  var x = arrayLikeOrIterable.length;
}
