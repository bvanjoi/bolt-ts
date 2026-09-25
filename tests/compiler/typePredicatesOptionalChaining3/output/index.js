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