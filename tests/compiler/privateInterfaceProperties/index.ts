// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privateInterfaceProperties.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface i1 { name:string; }
 
// should be an error 
class c1 implements i1 { private name:string; }
//~^ ERROR: Class 'c1' incorrectly implements interface 'i1'.
//~| ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
 
// should be ok 
class c2 implements i1 { public name:string; }
//~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
 