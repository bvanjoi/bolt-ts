// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/inferentialTypingWithFunctionType2.ts`, Apache-2.0 License

function identity<A>(a: A): A {
  return a;
}
var x = [1, 2, 3].map(identity)[0];
var x1: number = x;

var y0: number[] = [1,2,3].map(identity);
var y1: number = y0[0]
