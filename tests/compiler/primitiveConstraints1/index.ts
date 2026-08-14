// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/primitiveConstraints1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function foo1<T extends U, U>(t: T, u: U) { }
foo1<string, number>('hm', 1); // no error
//~^ ERROR: Type 'string' does not satisfy the constraint 'number'.
 
function foo2<T, U extends T>(t: T, u: U) { }
foo2<number, string>(1, 'hm'); // error
//~^ ERROR: Type 'string' does not satisfy the constraint 'number'.
