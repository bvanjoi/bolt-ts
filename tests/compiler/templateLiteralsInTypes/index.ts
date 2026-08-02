// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateLiteralsInTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration

const f = (hdr: string, val: number) => `${hdr}:\t${val}\r\n` as `${string}:\t${number}\r\n`;

f("x").foo;
//~^ ERROR: Expected 2 arguments, but got 1.
//~| ERROR: Property 'foo' does not exist on type '`${string}:\t${number}\r\n`'.

const a: `${string}:123` = "x:123456";
//~^ ERROR: Type 'string' is not assignable to type '`${string}:123`'.
