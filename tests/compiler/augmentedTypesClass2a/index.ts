// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypesClass2a.ts`, Apache-2.0 License

//@compiler-options: target=es2015

//// class then function
class c2 { public foo() { } } // error
//~^ ERROR: Class declaration cannot implement overload list for 'c2'.
function c2() { } // error
//~^ ERROR: Function with bodies can only merge with classes that are ambient.
var c2 = () => { }
//~^ ERROR: Duplicate identifier 'c2'.
