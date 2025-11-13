// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallFromClassThatDerivesFromGenericTypeButWithIncorrectNumberOfTypeArguments1.ts`, Apache-2.0 License

class A<T1, T2> {
    constructor(private map: (value: T1) => T2) {

    }
}

class B extends A<number> {
  //~^ ERROR: Generic type 'A<T1, T2>' requires 2 type arguments.
    constructor() { super(value => String(value)); }
}