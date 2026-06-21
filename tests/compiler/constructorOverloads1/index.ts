// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorOverloads1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {
    constructor(s: string);
    //~^ ERROR: Multiple constructor implementations are not allowed.
    constructor(n: number);
    //~^ ERROR: Multiple constructor implementations are not allowed.
    constructor(x: any) {
    //~^ ERROR: Multiple constructor implementations are not allowed.

    }
    constructor(x: any) {
    //~^ ERROR: Multiple constructor implementations are not allowed.

    }
    bar1() {  /*WScript.Echo("bar1");*/ }
    bar2() {  /*WScript.Echo("bar1");*/ }
}

var f1 = new Foo("hey");
var f2 = new Foo(0);
var f3 = new Foo(f1);
//~^ ERROR: No overload matches this call.
var f4 = new Foo([f1,f2,f3]);
//~^ ERROR: No overload matches this call.

f1.bar1();
f1.bar2();
