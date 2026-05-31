function Tagged(Base) {
  return class extends Base {
    _tag;
    constructor(...args) {super(...args);this._tag = '';}
  }
}
class A {
  toString() {
    return 'class A'
  }
}
class B extends Tagged(A) {
  toString() {
    return 'class B'
  }
}
class C extends A {
  toString() {
    return 'class C'
  }
}