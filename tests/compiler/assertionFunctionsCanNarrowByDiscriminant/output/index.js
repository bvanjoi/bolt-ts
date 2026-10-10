// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assertionFunctionsCanNarrowByDiscriminant.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: strict
//@run-fail
var animal = {
  type: 'cat',
  canMeow: true  
};
assertEqual(animal.type, 'cat');
animal.canMeow;// since is cat, should not be an error

var animalOrUndef = {
  type: 'cat',
  canMeow: true  
};
assertEqual(animalOrUndef.type, 'cat');
animalOrUndef.canMeow;