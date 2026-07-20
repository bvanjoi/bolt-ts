// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/illegalSuperCallsInConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Base {
    x: string;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
}
 
class Derived extends Base {
    constructor() {
      //~^ ERROR: Constructors for derived classes must contain a 'super' call.
        var r2 = () => super();
                //~^ ERROR: Super calls are not permitted outside constructors or in nested functions inside constructors.
        var r3 = () => { super(); }
                //~^ ERROR: Super calls are not permitted outside constructors or in nested functions inside constructors.
        var r4 = function () { super(); }
                //~^ ERROR: Super calls are not permitted outside constructors or in nested functions inside constructors.
        var r5 = {
            get foo() {
                super();
                //~^ ERROR: Super calls are not permitted outside constructors or in nested functions inside constructors.
                return 1;
            },
            set foo(v: number) {
                super();
                //~^ ERROR: Super calls are not permitted outside constructors or in nested functions inside constructors.
            }
        }
    }
}

