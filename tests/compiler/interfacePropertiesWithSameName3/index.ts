// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfacePropertiesWithSameName3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface D { a: number; }
interface E { a: string; }
interface F extends E, D { } // error
//~^ ERROR: Interface 'F' cannot simultaneously extend types 'E' and 'D'.

class D2 { a: number; }
//~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
class E2 { a: string; }
//~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
interface F2 extends E2, D2 { } // error
//~^ ERROR: Interface 'F2' cannot simultaneously extend types 'E2' and 'D2'.