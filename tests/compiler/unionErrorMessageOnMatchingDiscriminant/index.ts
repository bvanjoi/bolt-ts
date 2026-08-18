// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionErrorMessageOnMatchingDiscriminant.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type A = {
    type: 'a',
    data: { a: string }
};

type B = {
    type: 'b',
    data: null
};

type C = {
    type: 'c',
    payload: string
};

type Union = A | B | C;

// error
const foo: Union = {
    type: 'a',
    data: null
    //~^ ERROR: Type 'null' is not assignable to type '{ a: string; }'.
};