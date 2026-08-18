// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInAccessors.ts`, Apache-2.0 License
class GetterOnly {
  get Value() {
    var fn = () => (this);
    return '';
  }
  set Value(val) {}
}
class SetterOnly {
  get Value() {
    return '';
  }
  set Value(val) {
    var fn = () => (this);
  }
}
class GetterAndSetter {
  get Value() {
    var fn = () => (this);
    return '';
  }
  set Value(val) {
    var fn = () => (this);
  }
}