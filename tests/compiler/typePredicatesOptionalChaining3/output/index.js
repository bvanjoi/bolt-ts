// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicatesOptionalChaining3.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function getBreedSizeWithoutFunction(animal) {
  if (animal.breed.size != null) {
    return animal.breed.size;
  } else {
    return undefined;
  }
  
}
function getBreedSizeWithFunction(animal) {
  if (!isNil(animal.breed.size)) {
    return animal.breed.size;
  } else {
    return undefined;
  }
  
}