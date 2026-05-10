class NumCoercible {
  a;
  constructor(a) {this.a = a;}
  valueOf() {
    return this.a
  }
}
export function extent(array) {
  return [undefined, undefined]
}
var extentMixed;
extentMixed = extent([new NumCoercible(10), 13, '12', true]);