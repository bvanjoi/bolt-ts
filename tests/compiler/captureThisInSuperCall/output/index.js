// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/captureThisInSuperCall.ts`, Apache-2.0 License
class A {
  constructor(p) {}
}
class B extends A {
  constructor() {super({
          test: () => (this.someMethod())      
    });}
  someMethod() {}
}