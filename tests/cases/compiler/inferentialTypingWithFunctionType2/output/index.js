// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inferentialTypingWithFunctionType2.ts`, Apache-2.0 License
function identity(a) {
  return a
}
var x = [1, 2, 3].map(identity)[0];
var x1 = x;
var y0 = [1, 2, 3].map(identity);
var y1 = y0[0];