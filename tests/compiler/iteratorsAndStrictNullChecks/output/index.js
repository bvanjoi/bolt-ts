// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/iteratorsAndStrictNullChecks.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
for ( var x of ['a', 'b']) {
  x.substring;
}
var xs = [1, 2, 3];
var ys = [4, 5];
xs.push(...ys);