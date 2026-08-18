// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parameterReferenceInInitializer1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function fn<a>(y: Y, set: (y: Y, x: number) => void): a {
    return undefined;
    //~^ ERROR: Type 'undefined' is not assignable to type 'a'.
}
interface Y { x: number }

class C {
    constructor(
        y: Y,
        public x = fn(y, (y, x) => y.x = x) // expected to work, but actually doesn't
    ) {
    }
}