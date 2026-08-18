// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/omitTypeTests01.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
export function getBarA(bar) {
  return bar.a;
}
export function getBazA(baz) {
  return baz.a;
}