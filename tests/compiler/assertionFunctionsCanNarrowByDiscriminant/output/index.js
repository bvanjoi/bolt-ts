// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assertionFunctionsCanNarrowByDiscriminant.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: strict
var animal = {
  type: 'cat',
  canMeow: true  
};
assertEqual(animal.type, 'cat');
animal.canMeow;
var animalOrUndef = {
  type: 'cat',
  canMeow: true  
};
assertEqual(animalOrUndef.type, 'cat');
animalOrUndef.canMeow;