// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeInferenceLiteralUnion.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// Repro from #10901
/**
 * Administrivia: JavaScript primitive types and Date
 */
/**
 * Administrivia: anything with a valueOf(): number method is comparable, so we allow it in numeric operations
 */
// Not very useful, but meets Numeric
class NumCoercible {
  a;
  constructor(a) {this.a = a;}
  valueOf() {
    return this.a;
  }
}
export /**
 * Return the min and max simultaneously.
 */
function extent(array) {
  return [undefined, undefined];
}
var extentMixed;
extentMixed = extent([new NumCoercible(10), 13, '12', true]);