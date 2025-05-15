// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInAccessors.ts`, Apache-2.0 License
// this capture only in getter
class GetterOnly {
  get Value() {
    var fn = () => this;
    return ""
  }
  set Value(val) {}
}
// this capture only in setter
class SetterOnly {
  get Value() {
    return ""
  }
  set Value(val) {
    var fn = () => this;
  }
}
// this capture only in both setter and getter
class GetterAndSetter {
  get Value() {
    var fn = () => this;
    return ""
  }
  set Value(val) {
    var fn = () => this;
  }
}