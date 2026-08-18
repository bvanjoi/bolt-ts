// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicatesCanNarrowByDiscriminant.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict

if (isOneOf(fruit.kind, ['apple', 'banana'])) {
  fruit.kind;
  fruit;
}


var kind = fruit2.kind;
if (isOneOf(kind, ['apple', 'banana'])) {
  fruit2.kind;
  fruit2;
}
