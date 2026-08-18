// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/subclassWithPolymorphicThisIsAssignable.ts`, Apache-2.0 License
//@compiler-options: target=es2015
export class Example {
  constructor() {this.test();}
  test() {}
}