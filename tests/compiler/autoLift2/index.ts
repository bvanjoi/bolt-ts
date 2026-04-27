// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/autoLift2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A

{
    constructor() {
        this.foo: any;
        //~^ ERROR: Declaration or statement expected.
        //~| ERROR: Cannot find name 'any'.
        //~| ERROR: Property 'foo' does not exist on type 'A'.
        this.bar: any;
        //~^ ERROR: Declaration or statement expected.
        //~| ERROR: Cannot find name 'any'.
        //~| ERROR: Property 'bar' does not exist on type 'A'.
    }


  baz() {

     this.foo = "foo";
    //~^ ERROR: Property 'foo' does not exist on type 'A'.

     this.bar = "bar";
    //~^ ERROR: Property 'bar' does not exist on type 'A'.

     [1, 2].forEach((p) => this.foo);
    //~^ ERROR: Property 'foo' does not exist on type 'A'.

     [1, 2].forEach((p) => this.bar);
    //~^ ERROR: Property 'bar' does not exist on type 'A'.
  }

}



var a = new A();

a.baz();


