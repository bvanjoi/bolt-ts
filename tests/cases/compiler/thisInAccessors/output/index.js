class GetterOnly {
  get Value() {
    var fn = () => this;
    return ''
  }
  set Value(val) {}
}
class SetterOnly {
  get Value() {
    return ''
  }
  set Value(val) {
    var fn = () => this;
  }
}
class GetterAndSetter {
  get Value() {
    var fn = () => this;
    return ''
  }
  set Value(val) {
    var fn = () => this;
  }
}