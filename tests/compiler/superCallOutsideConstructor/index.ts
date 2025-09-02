// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/superCallOutsideConstructor.ts`, Apache-2.0 License

class C {
    foo() { }
}
 
class D extends C {
    x = super(); 
    //~^ ERROR: Super calls are not permitted outside constructors or in nested functions inside constructors.
 
    constructor() {
        super();
 
        var y = () => {
            super(); 
            //~^ ERROR: Super calls are not permitted outside constructors or in nested functions inside constructors.
        }

        var y2 = function() {
            super();
            //~^ ERROR: Super calls are not permitted outside constructors or in nested functions inside constructors.
        }
    }
}
 
var d = new D();
