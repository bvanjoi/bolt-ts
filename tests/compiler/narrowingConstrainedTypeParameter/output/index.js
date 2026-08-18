// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingConstrainedTypeParameter.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function isPet(pet) {
  return typeof pet.name === 'string';
}
export function speak(pet, voice) {
  if (!isPet(pet)) {
    throw new Error('Expected "pet" to be a Pet')
  }
  
  return voice(pet);
}