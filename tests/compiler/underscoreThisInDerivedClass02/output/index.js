// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/underscoreThisInDerivedClass02.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// Original test intent:
// Errors on '_this' should be reported in derived constructors,
class C {
  constructor() {return {};}
}
class D extends C {
  constructor() {super();var _this = 'uh-oh?';}
}