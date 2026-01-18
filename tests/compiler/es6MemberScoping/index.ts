// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/es6MemberScoping.ts`, Apache-2.0 License

class Foo {
    constructor(store: string) { }

    public foo() {
        return this._store.length; 
    }
    public _store = store; // should be an error.
    //~^ ERROR: Cannot find name 'store'.
}

class Foo2 {
 
  static Foo2():number { return 0; } // should not be an error
 
}
