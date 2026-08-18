// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mixingApparentTypeOverrides.ts`, Apache-2.0 License
function Tagged(Base) {
  return class extends Base {
    _tag;
    constructor(...args) {super(...args);this._tag = '';}
  };
}
class A {
  toString() {
    return 'class A';
  }
}
class B extends Tagged(A) {
  toString() {
    return 'class B';
  }
}
class C extends A {
  toString() {
    return 'class C';
  }
}