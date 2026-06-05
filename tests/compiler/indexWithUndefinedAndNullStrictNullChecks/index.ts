// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexWithUndefinedAndNullStrictNullChecks.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

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
//~| ERROR: Variable 'n' is used before being assigned.
str = n[null];
//~^ ERROR: Type 'null' cannot be used as an index type.
//~| ERROR: Variable 'n' is used before being assigned.
let num: number = s[undefined];
//~^ ERROR: Type 'undefined' cannot be used as an index type.
//~| ERROR: Variable 's' is used before being assigned.
num = s[null];
//~^ ERROR: Type 'null' cannot be used as an index type.
//~| ERROR: Variable 's' is used before being assigned.


s[false];
//~^ ERROR: Type 'false' cannot be used as an index type.
//~| ERROR: Variable 's' is used before being assigned.
s[true]
//~^ ERROR: Type 'true' cannot be used as an index type.
//~| ERROR: Variable 's' is used before being assigned.
