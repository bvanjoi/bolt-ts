// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/strictTypeofUnionNarrowing.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function stringify1(anything) {
  return typeof anything === 'string' ? anything.toUpperCase() : '';
}
function stringify2(anything) {
  return typeof anything === 'string' ? anything.toUpperCase() : '';
}
function stringify3(anything) {
  return typeof anything === 'string' ? anything.toUpperCase() : '';
}
function stringify4(anything) {
  return typeof anything === 'string' ? anything.toUpperCase() : '';
}