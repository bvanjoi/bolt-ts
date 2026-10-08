// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/varianceProblingAndZeroOrderIndexSignatureRelationsAlign2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
class Left {
  _tag = 'Left';
  _A;
  _L;
  constructor(value) {}
  /** The given function is applied if this is a `Right` */
  map(f) {
    return this;
  }
  ap(fab) {
    return null;
  }
}
class Right {
  _tag = 'Right';
  _A;
  _L;
  constructor(value) {}
  map(f) {
    return new Right(f(this.value));
  }
  ap(fab) {
    return null;
  }
}
class Type {
  _A;
  _O;
  _I;
  constructor(/** a unique name for this codec */
  name, /** a custom type guard */
  is, /** succeeds if a value of type I can be decoded to a value of type A */
  validate, /** converts a value of type A to a value of type O */
  encode) {}
  /** a version of `validate` with a default context */
  decode(i) {
    return null;
  }
}
var tmp1 = null;
function tmp2(n) {}
// tmp2(tmp1); // uncommenting this line removes a type error from a completely unrelated line ?? (see test 1, needs to behave the same)
class Server {}
export class MyServer extends Server {}