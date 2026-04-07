// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/thisPredicateInObjectLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

// Should be OK
const foo2 = {
    isNumber(): this is { b: string } {
        return true;
    },
};

// Still an error
const foo3 = {
    isNumber(x: any): x is this {
        //~^ ERROR: A 'this' type is available only in a non-static member of a class or interface.
        return true;
    },
}

function f<const T>() {}
