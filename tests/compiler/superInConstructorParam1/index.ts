// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superInCatchBlock1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class B {
  public foo(): number {
    return 0;
  }
}

class C extends B {
  constructor(a = super.foo()) {
    //~^ ERROR: Constructors for derived classes must contain a 'super' call.
    //~| ERROR: 'super' must be called before accessing a property of 'super' in the constructor of a derived class.
    //~| ERROR: 'super' cannot be referenced in constructor arguments.
  }
}
