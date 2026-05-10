// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypingWithFixedTypeParameters1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false


declare var f10: <T>(x: T, b: () => (a: T) => void, y: T) => T;
f10('', () => a => a.foo, ''); // a is ""
//~^ ERROR: Property 'foo' does not exist on type 'string'.
var r9 = f10('', () => (a => a.foo), 1); // error
//~^ ERROR: Property 'foo' does not exist on type '""'.
//~| ERROR: Argument of type 'number' is not assignable to parameter of type '""'.