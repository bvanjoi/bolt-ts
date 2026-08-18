// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/voidIsInitialized.ts`, Apache-2.0 License
//@compiler-options: target=esnext
var x = undefined;
var y = undefined;
if (typeof x === 'undefined') {
  x;
}

if (typeof y !== 'undefined') {
  y;
}
