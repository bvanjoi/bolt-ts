// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadIdenticalTypesRemoved.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function clonePet(pet, fullCopy) {
  return {
      name: pet.name,
    kind: pet.kind,
    ...(fullCopy && pet)    
  };
}
function billOwner(pet) {
  return {
      ...(pet.owner && pet),
    paid: false    
  };
}