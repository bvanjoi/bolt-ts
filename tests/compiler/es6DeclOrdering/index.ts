// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6DeclOrdering.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Bar {

   //public bar() { }

   public foo() {
       return this._store.length;
      //~^ ERROR: Property '_store' does not exist on type 'Bar<Bar>'.
   }

   constructor(store: string) {
       this._store = store; // this is an error for some reason? Unresolved symbol store
      //~^ ERROR: Property '_store' does not exist on type 'Bar<Bar>'.
   } 
}

