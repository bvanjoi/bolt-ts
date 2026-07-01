// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceShouldFailOnEvolvingArrays.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=es2015

// repro from https://github.com/Microsoft/TypeScript/issues/25675
// The type of `arg` blocks inference but simplifies to T.
function logLength<T extends string, U extends string>(arg: { [K in U]: T }[U]): T {
    console.log(arg.length);
    return arg;
}
logLength(42);  // error
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
let z;
z = logLength(42);  // no error; T is inferred as `any`
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.

function logFirstLength<T extends string[], U extends string>(arg: { [K in U]: T }[U]): T {
    console.log(arg[0].length);
    return arg;
}
logFirstLength([42]);  // error
//~^ ERROR: Type 'number' is not assignable to type 'string'.
let zz = [];
zz.push(logLength(42));  // no error; T is inferred as `any`
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
zz = logFirstLength([42]);  // no error; T is inferred as `any[]`
//~^ ERROR: Type 'number' is not assignable to type 'string'.