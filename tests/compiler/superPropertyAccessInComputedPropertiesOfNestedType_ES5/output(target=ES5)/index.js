// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superPropertyAccessInComputedPropertiesOfNestedType_ES5.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
class A {
  foo() {
    return 1;
  }
}
class B extends A {
  foo() {
    return 2;
  }
  bar() {
    return class {
      [super.foo()]() {
        return 100;
      }
    };
  }
}