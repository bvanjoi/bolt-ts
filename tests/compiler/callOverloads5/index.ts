// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/callOverloads5.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function Foo():Foo; // error
//~^ ERROR: Function with bodies can only merge with classes that are ambient.
//~| ERROR: Function implementation is missing or not immediately following the declaration.
function Foo(s:string):Foo; // error
//~^ ERROR: Function with bodies can only merge with classes that are ambient.
class Foo { // error
//~^ ERROR: Class declaration cannot implement overload list for 'Foo'.
	bar1(s:string);
	bar1(n:number);
    bar1(a:any) { /*WScript.Echo(a);*/ }
    constructor(x: any) {
        // WScript.Echo("Constructor function has executed");
    }
}
//class Foo(s: String);

var f1 = new Foo("hey");


f1.bar1("a");
Foo();
Foo("s");
