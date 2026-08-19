function isPet(pet) {
  return typeof pet.name === 'string';
}
export function speak(pet, voice) {
  if (!isPet(pet)) {
    throw new Error('Expected "pet" to be a Pet')
  }
  
  return voice(pet);
}