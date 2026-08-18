// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyOrdering.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {
    constructor(store: string) { }
      public foo() {
            return this._store.length;   // shouldn't be an error
      }
      public _store = store; // no repro if this is first line in class body
      //~^ ERROR: Cannot find name 'store'.


      public bar() { return this.store; } // should be an error
      //~^ ERROR: Property 'store' does not exist on type 'Foo<Foo>'.

}

class Bar {
      public foo() {

            return this._store.length;   // shouldn't be an error
            //~^ ERROR: Property '_store' does not exist on type 'Bar<Bar>'.

      }
    constructor(store: string) {
        this._store = store;
        //~^ ERROR: Property '_store' does not exist on type 'Bar<Bar>'.
    }
}