// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/bases.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface I {
    x;
}

class B {
    constructor() {
        this.y: any;
        //~^ ERROR: Unexpected keyword or identifier.
        //~| ERROR: Declaration or statement expected.
        //~| ERROR: Cannot find name 'any'.
        //~| ERROR: Property 'y' does not exist on type 'B<B>'.
    }
}

class C extends B implements I {
  //~^ ERROR: Property 'x' is missing.        
    constructor() {
      //~^ ERROR: Constructors for derived classes must contain a 'super' call.
        this.x: any;
        //~^ ERROR: Unexpected keyword or identifier.
        //~| ERROR: Declaration or statement expected.
        //~| ERROR: Cannot find name 'any'.
        //~| ERROR: Property 'x' does not exist on type 'C<C>'.
        //~| ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
    }
}

new C().x;
//~^ ERROR: Property 'x' does not exist on type 'C'.
new C().y;
//~^ ERROR: Property 'y' does not exist on type 'C'.