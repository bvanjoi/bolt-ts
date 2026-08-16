// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constIndexedAccess.ts`, Apache-2.0 License

//@compiler-options: target=es2015

const enum numbers {
    zero,
    one
}

interface indexAccess {
    0: string;
    1: number;
}

let test: indexAccess;

let s = test[0];
//~^ ERROR: Variable 'test' is used before being assigned.
let n = test[1];
//~^ ERROR: Variable 'test' is used before being assigned.

let s1 = test[numbers.zero];
//~^ ERROR: Variable 'test' is used before being assigned.
let n1 = test[numbers.one];
//~^ ERROR: Variable 'test' is used before being assigned.

let s2 = test[numbers["zero"]];
//~^ ERROR: Variable 'test' is used before being assigned.
let n2 = test[numbers["one"]];
//~^ ERROR: Variable 'test' is used before being assigned.

enum numbersNotConst {
    zero,
    one
}

let s3 = test[numbersNotConst.zero];
//~^ ERROR: Variable 'test' is used before being assigned.
let n3 = test[numbersNotConst.one];
//~^ ERROR: Variable 'test' is used before being assigned.
