// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/libMembers.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var s="hello";
s.substring(0);
s.substring(3,4);
s.subby(12);   // error unresolved
//~^ ERROR: Property 'subby' does not exist on type 'string'.
String.fromCharCode(12);
namespace M {
    export class C {
    }
    var a=new C[];
    //~^ ERROR: An element access expression should take an argument.
    a.length;
    a.push(new C());
    (new C()).prototype;
    //~^ ERROR: Property 'prototype' does not exist on type 'M.C'.
}

