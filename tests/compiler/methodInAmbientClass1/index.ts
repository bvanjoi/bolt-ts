// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/methodInAmbientClass1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

    declare class Foo {
        fn(): boolean {
          //~^ ERROR: A function whose declared type is neither 'undefined', 'void', nor 'any' must return a value.
          //~| ERROR: An implementation cannot be declared in ambient contexts.
        }
    }