// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceImplementation5.ts`, Apache-2.0 License
class C1 {
  get getset1() {
    return 1;
  }
}
class C2 {
  set getset1(baz) {}
}
class C3 {
  get getset1() {
    return 1;
  }
  set getset1(baz) {}
}
class C4 {
  get getset1() {
    var x;
    return x;
  }
}
class C5 {
  set getset1(baz) {}
}
class C6 {
  set getset1(baz) {}
  get getset1() {
    var x;
    return x;
  }
}