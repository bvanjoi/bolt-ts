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
  constructor(name, is, validate, encode) {}
  /** a version of `validate` with a default context */
  decode(i) {
    return null;
  }
}
var tmp1 = null;
function tmp2(n) {}
tmp2(tmp1);
class Server {}
export class MyServer extends Server {}