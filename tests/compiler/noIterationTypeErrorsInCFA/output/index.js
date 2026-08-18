// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noIterationTypeErrorsInCFA.ts`, Apache-2.0 License
//@compiler-options: target=esnext
export function doRemove(dds) {
  if (!Array.isArray(dds)) {
    dds = [dds];
  }
  
  for ( var n of dds) {
    n.d();
  }
  return dds;
}