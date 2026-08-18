// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/narrowUnknownByTypeofObject.ts`, Apache-2.0 License
function foo(x) {
  if (typeof x === 'object') {
    x;
  }
  
}