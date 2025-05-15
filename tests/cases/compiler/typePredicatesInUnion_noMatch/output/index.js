


// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/typePredicatesInUnion_noMatch.ts`, Apache-2.0 License
function f(o, x, y) {
  if (o.pred(x, y)) {
    x;
    y;
  }
  
}