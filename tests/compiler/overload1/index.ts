// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overload1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace O {
    export class A {
        
    }

    export class B extends A {
    }

    export class C extends B {
        
    }

    export interface I {
        f(s:string):number;
        f(n:number):string;
        g(n1:number,n2:number):number;
        g(n:number):string;
        g(a:A):C;
        g(c:C):string;
        h(s1:string,s2:number):string;
        h(s1:number,s2:string):number;
    }
}

declare var x:O.I;

var e:string=x.g(new O.A()); // matches overload but bad assignment
//~^ ERROR: Type 'O.C' is not assignable to type 'string'.
var y:string=x.f(3); // good
y=x.f("nope"); // can't assign number to string
//~^ ERROR: Type 'number' is not assignable to type 'string'.
var z:string=x.g(x.g(3,3)); // good
z=x.g(2,2,2); // no match
//~^ ERROR: Expected 1-2 arguments, but got 3.
//~| ERROR: Type 'number' is not assignable to type 'string'.
z=x.g(); // no match
//~^ ERROR: Expected 1-2 arguments, but got 0.
//~| ERROR: Type 'number' is not assignable to type 'string'.
z=x.g(new O.B()); // ambiguous (up and down conversion)
//~^ ERROR: Type 'O.C' is not assignable to type 'string'.
z=x.h(2,2); // no match
//~^ ERROR: No overload matches this call.
z=x.h("hello",0); // good

var v=x.g;

