// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexWithUndefinedAndNull.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks=false

interface N {
    [n: number]: string;
}
interface S {
    [s: string]: number;
}
let n: N;
let s: S;
let str: string = n[undefined];
//~^ ERROR: Type 'undefined' cannot be used as an index type.
str = n[null];
//~^ ERROR: Type 'null' cannot be used as an index type.
let num: number = s[undefined];
//~^ ERROR: Type 'undefined' cannot be used as an index type.
num = s[null];
//~^ ERROR: Type 'null' cannot be used as an index type.


s[false];
//~^ ERROR: Type 'false' cannot be used as an index type.
s[true]
//~^ ERROR: Type 'true' cannot be used as an index type.
