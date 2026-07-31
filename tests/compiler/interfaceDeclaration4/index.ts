// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceDeclaration4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// Import this module when test harness supports external modules. Also remove the internal module below.
// import Foo = require("interfaceDeclaration5")
namespace Foo {
    export interface I1 { item: string; }
    export class C1 { }
}

class C1 implements Foo.I1 {
	public item:string;
  //~^ ERROR: Property 'item' has no initializer and is not definitely assigned in the constructor.
}

// Allowed
interface I2 extends Foo.I1 {
	item:string;
}

// Negative Case
interface I3 extends Foo.I1 {
  //~^ ERROR: Interface 'I3' incorrectly extends interface 'Foo.I1'.
    item:number;
}

interface I4 extends Foo.I1 {
    token:string;
}

// Err - not implemented item
class C2 implements I4 {
  //~^ ERROR: Property 'item' is missing.
    public token: string;
    //~^ ERROR: Property 'token' has no initializer and is not definitely assigned in the constructor.
}

interface I5 extends Foo { }

// Negative case
interface I6 extends Foo.C1 { }

class C3 implements Foo.I1 { }
//~^ ERROR: Property 'item' is missing.

// Negative case 
interface Foo.I1 { }
//~^ ERROR: Expected '{'.
//~| ERROR: Expected '}'.
//~| ERROR: Cannot find name 'I1'.
//~| ERROR: Property or signature expected.
//~| ERROR: Property or signature expected.
