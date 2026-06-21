// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayAssignmentTest4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C3 {
    CM3M1() { return 3;}
}


/*

This behaves unexpectedly with teh following types:

Type 1 of any[]:

* Type 2 of the following throws an error but shouldn't: () => void[], SomeClass[], and {one: 1}[].

* Type 2 of the following doesn't throw an error but should: {one: 1}, new() => SomeClass, SomeClass.

*/
var c3 : C3 = new C3();
var o1 = {one : 1};

var arr_any: any[] = [];

arr_any = function () { return null;} // should be an error - is
//~^ ERROR: Type '() => any' is not assignable to type 'any[]'.
arr_any = c3; // should be an error - is
//~^ ERROR: ype 'C3' is missing the following properties from type 'any[]': length, pop, and 27 more.
