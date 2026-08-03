// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privateInstanceVisibility.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Test {
 
    export class Example {
 
        private someNumber: number;
    //~^ ERROR: Property 'someNumber' has no initializer and is not definitely assigned in the constructor.
        

        public doSomething() {
 
            var that = this;                      

            function innerFunction() {
                
                var num = that.someNumber;
 
            }
 
        }        

    }
 
}



class C {

    private x: number;
    //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.

    getX() { return this.x; }

    clone(other: C) {
        this.x = other.x;

    }
}
