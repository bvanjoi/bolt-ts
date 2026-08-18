// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typePredicatesInUnion.ts`, Apache-2.0 License
function f(o, x) {
  if (o.pred(x)) {
    x;
  }
  
}