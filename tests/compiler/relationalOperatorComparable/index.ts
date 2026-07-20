// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/relationalOperatorComparable.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f(onethree: 1 | 3, two: 2) {
    const t = true;
    const f = false;
    let a1 = onethree < two; // ok
    let a2 = onethree < true; // error, number and boolean
    //~^ ERROR: Operator '<' cannot be applied to types 'number' and 'boolean'.
    let a3 = onethree <= false; // error, number and boolean
    //~^ ERROR: Operator '<=' cannot be applied to types 'number' and 'boolean'.
    let a4 = onethree >= t; // error, number and boolean
    //~^ ERROR: Operator '>=' cannot be applied to types 'number' and 'boolean'.
    let a5 = onethree > f; // error, number and boolean
    //~^ ERROR: Operator '>' cannot be applied to types 'number' and 'boolean'.
    let a6 = true < onethree; // error, boolean and number
    //~^ ERROR: Operator '<' cannot be applied to types 'boolean' and 'number'.
    let a7 = false < two; // error, boolean and number
    //~^ ERROR: Operator '<' cannot be applied to types 'boolean' and 'number'.
    let a8 = 'foo' < onethree; // error, string and number
    //~^ ERROR: Operator '<' cannot be applied to types 'string' and 'number'.
    let a9 = onethree < 1; // ok
    let a10 = 1 < two; // ok
    let a11 = 2 < 1; // ok
}
