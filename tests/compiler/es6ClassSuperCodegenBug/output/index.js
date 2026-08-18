// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ClassSuperCodegenBug.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  constructor(str1, str2) {}
}
class B extends A {
  constructor() {if (true) {
      super('a1', 'b1');
    } else {
      super('a2', 'b2');
    }
    }
}