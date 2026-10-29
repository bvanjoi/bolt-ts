// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictNullNotNullIndexTypeShouldWork.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: strictPropertyInitialization=false
class Test {
  attrs;
  m() {
    this.attrs.params.name;
  }
}
class FooClass {
  properties;
  foo() {
    var {foo = 42} = this.properties;
    return foo;
  }
}
class Test2 {
  attrs;
  m() {
    return this.attrs.params;// Return type should maintain relationship with `T` after being not-null-asserted, ideally
    
  }
}