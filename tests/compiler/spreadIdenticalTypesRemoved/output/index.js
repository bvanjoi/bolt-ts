function clonePet(pet, fullCopy) {
  return {
      name: pet.name,
    kind: pet.kind,
    ...(fullCopy && pet)    
  }
}
function billOwner(pet) {
  return {
      ...(pet.owner && pet),
    paid: false    
  }
}