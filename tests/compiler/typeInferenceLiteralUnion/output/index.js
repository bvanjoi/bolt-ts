// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeInferenceLiteralUnion.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// Repro from #10901
class NumCoercible {
  a;
  constructor(a) {this.a = a;}
  valueOf() {
    return this.a;
  }
}
export function extent(array) {
  return [undefined, undefined];
}
var extentMixed;
extentMixed = extent([new NumCoercible(10), 13, '12', true]);