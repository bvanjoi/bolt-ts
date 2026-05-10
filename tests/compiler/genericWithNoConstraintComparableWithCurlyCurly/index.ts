// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericWithNoConstraintComparableWithCurlyCurly.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=true

function foo<T>() {
    let x = {};
    x as T;
}

function bar<T extends unknown>() {
    let x = {};
    x as T;
}

function baz<T extends {}>() {
    let x = {};
    x as T;
}

function bat<T extends object>() {
    let x = {};
    x as T;
}

function no<T extends null | undefined>() {
    let x = {};
    x as T; // should error
    //~^ ERROR: Conversion of type '{ }' to type 'T' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.
}

function yes<T extends object | null | undefined>() {
    let x = {};
    x as T;
}