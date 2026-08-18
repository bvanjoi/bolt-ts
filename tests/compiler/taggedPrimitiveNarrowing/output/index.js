// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/taggedPrimitiveNarrowing.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
function getHashLength(hash) {
  if (typeof hash !== 'string') {
    throw new Error('This doesn\'t look like a hash')
  }
  
  return hash.length;
}
function getHashLength2(hash) {
  if (typeof hash !== 'string') {
    throw new Error('This doesn\'t look like a hash')
  }
  
  return hash.length;
}