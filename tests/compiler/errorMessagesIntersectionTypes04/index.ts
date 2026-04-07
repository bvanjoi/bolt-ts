// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/errorMessagesIntersectionTypes04.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface A {
    a;
}

interface B {
    b;
}

function f<T, U extends A, V extends U>(): void {
    let num: number;
    let bool: boolean;
    let str: string;

    let a_and_b: A & B;
    let num_and_bool: number & boolean;

    num = a_and_b;
    //~^ ERROR: Type 'A & B' is not assignable to type 'number'.
    bool = a_and_b;
    //~^ ERROR: Type 'A & B' is not assignable to type 'boolean'.
    str = a_and_b;
    //~^ ERROR: Type 'A & B' is not assignable to type 'string'.

    str = num_and_bool;
}