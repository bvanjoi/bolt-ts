// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/superAccessInFatArrow1.ts`, Apache-2.0 License

module test {
  export class A {
      foo() {
      }
  }
  export class B extends A {
      bar(callback: () => void ) {
      }
      runme() {
          this.bar(() => {
              super.foo();
          });
      }
  }
}