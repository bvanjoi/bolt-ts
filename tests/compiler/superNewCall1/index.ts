// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superNewCall1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class A<T1, T2> {
    constructor(private map: (value: T1) => T2) {

    }
}

class B extends A<number, string> {
    constructor() {
      //~^ ERROR: Constructors for derived classes must contain a 'super' call.
        new super(value => String(value));
        //~^ ERROR: This expression is not constructable.
        //~| ERROR: 'super' must be called before accessing a property of 'super' in the constructor of a derived class.
    }
}