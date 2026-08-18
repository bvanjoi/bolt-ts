// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/gettersAndSettersTypesAgree.ts`, Apache-2.0 License
class C {
  get Foo() {
    return 'foo';
  }
  set Foo(foo) {}
  get Bar() {
    return 'foo';
  }
  set Bar(bar) {}
}
var o1 = {
  get Foo() {
    return 0;
  },
  set Foo(val) {}  
};
var o2 = {
  get Foo() {
    return 0;
  },
  set Foo(val) {}  
};