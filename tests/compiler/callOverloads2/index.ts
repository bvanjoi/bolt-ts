// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/callOverloads2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Foo { // error
  //~^ ERROR: Class declaration cannot implement overload list for 'Foo'.
    bar1() { /*WScript.Echo("bar1");*/ }

    constructor(x: any) {
        // WScript.Echo("Constructor function has executed");
    }
}

function Foo(); // error
//~^ ERROR: Function with bodies can only merge with classes that are ambient.

function F1(s:string) {return s;} // error
//~^ ERROR: Function implementation name must be 'Foo'.
//~| ERROR: Duplicate function implementation.
function F1(a:any) { return a;} // error
//~^ ERROR: Duplicate function implementation.

function Goo(s:string); // error - no implementation
//~^ ERROR: Function implementation is missing or not immediately following the declaration.

declare function Gar(s:String); // expect no error

var f1 = new Foo("hey");


f1.bar1();
Foo();
