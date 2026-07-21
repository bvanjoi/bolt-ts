class Left {
  _tag = 'Left';
  _A;
  _L;
  constructor(value) {}
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
  decode(i) {
    return null;
  }
}
var tmp1 = null;
function tmp2(n) {}
class Server {}
export class MyServer extends Server {}